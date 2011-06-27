package debugginout.labassistant
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb._
  import mongodb.Limit
  import json._
    import Extraction.decompose
    import JsonDSL._

  import http._
    import js._
      import JsCmds._    
      import JE._
    import provider._
    import LiftRules._        
    import SHtml._

import java.util.Date
import debugginout.labassistant.lib._
import Helpers._

import debugginout.labassistant.model._

object session extends SessionVar[Box[UserSession]](Empty)

case class ShowLoginError(message:String) extends JsCmd {
  override val toJsCmd = Call("showLoginError", message).toJsCmd
}

object UserSessions { 

  def snippetHandlers : SnippetPF = {
    case List("admin-required") => kickIfNotAdmin _
    case List("login-required") => kickIfNotLogged _
    case List("instructor-only") => showIfInstructor _
    case List("admin-only") => showIfAdmin _
  }

  def kickIfNotAdmin(xhtml:NodeSeq) : NodeSeq = {
    if (! session.is.isDefined || ! session.is.open_!.user.admin_?)
      S.redirectTo("/")

    xhtml
  }
  
  def kickIfNotLogged(xhtml:NodeSeq) : NodeSeq = {
    if (! session.is.isDefined)
      S.redirectTo("/")

    xhtml
  }

  def showIfAdmin(xhtml:NodeSeq) : NodeSeq = {
    {
      for {
        session <- session.is
        user = session.user if user.admin_?
      } yield {
        xhtml
      }
    } openOr
      NodeSeq.Empty
  }

  def showIfInstructor(xhtml:NodeSeq) : NodeSeq = {
    {
      for {
        session <- session.is
        user = session.user if user.instructor_?
      } yield {
        xhtml
      }
    } openOr
      NodeSeq.Empty
  }
  

  def showIfRole(role:String)(xhtml:NodeSeq) : NodeSeq = {
    {
      for {
        session <- session.is
        user = session.user if user.is_?(role)
      } yield {
        xhtml
      }
    } openOr
      NodeSeq.Empty
    
  }

 /**
   * Logs the user in and returns a Box. If the login was successful, a Full
   * box with JsCmds to run client-side is returned; otherwise, a Failure is
   * returned.
   */
  def logUserIn(user:Box[User], postLoginAction:JsCmd) : Box[JsCmd] = {
    val userSession =
      for {
        user <- user
        ip = S.getRequestHeader("X-Forwarded-For")
        session = UserSession(userId = user._id, ip = ip, valid = true)
      } yield {
        session.save

        /*val sessionCookie =
          HTTPCookie("debuggin-lab-ass", session.uniqueId).
            setMaxAge((((session.expiresAt:TimeSpan) - millis) / 1000L).toInt).
            setPath("/")
        S.addCookie(sessionCookie)
        */
        session
      }

    session(userSession)

    user.map { user =>
      //Alert("session created") &
      postLoginAction
    } or {
      Failure("Session creation failed.")
    }
  }

  def userFailedLogin(userId:String) : Box[Int] = {
    //create failed usersession
    val ip = S.getRequestHeader("X-Forwarded-For")
    val session = UserSession(userId = userId.toLowerCase, ip = ip, valid = false)
    session.save

    val user = User.find("_id" -> userId.toLowerCase)

    user match {
      case Some(user) =>
        val latestSessions = UserSession.findAll("userId" -> userId.toLowerCase, "createdAt" -> -1, Limit(UserSession.SUSPEND_THRESHOLD))
        
        val numberOfConsecutiveFails = latestSessions.prefixLength(! _.valid)          
        if (numberOfConsecutiveFails == UserSession.SUSPEND_THRESHOLD) //suspend
          User.update("_id" -> userId.toLowerCase, "$set" -> ("status" -> User.Status.SUSPENDED))
        Full(numberOfConsecutiveFails)
      case _ =>
        //invalid user
        Failure("User does not exist")
    }

    
  }

}

class UserSessions {

  def loginForm = {
    var username = ""
    var password = ""

    def validateLogin = {
      val defaultError = "Check your username and password."

      val user = (User.forLogin(username, password) ?~ defaultError)
            .filterMsg("Your account has been temporarily suspended.")(! _.suspended_?)

      printBox(user)
      user match {
        case Full(user) =>
          val actions =
            UserSessions.logUserIn(Full(user),
                                   RedirectTo("/home"))

          actions match {
            case Full(actions) => actions
            case Failure(message, _, _) => ShowLoginError(message)
            case Empty => throw new Exception("Uh... Unexpected Empty there, son.")
          }
        case Failure(message, _, _) =>
          val numberOfFails = UserSessions.userFailedLogin(username)
          numberOfFails match {
            case Full(numFails) =>
              val response = {
                if (numFails == UserSession.SUSPEND_THRESHOLD)
                  "You have failed to login " + numFails + " consecutive times.\nYour account has been temporarily suspended."
                else
                  message + "\nYou have failed to login " + numFails + " consecutive times."              }
              Alert(response) //ShowLoginError
            case _ =>
              Alert(message)
          }
        case Empty =>
          Alert("Shouldn't really be empty here...")
      }
    }

    val processContents =
      ".username" #> text("", (value) => username = value) &
      ".password" #> SHtml.password("", (value) =>  password = value) &
      ".submit-button" #> ajaxSubmit("Sign In", validateLogin _)


    "*" #> { contents:NodeSeq =>
      ajaxForm(
        processContents(contents)
      )
    }
  }
}

