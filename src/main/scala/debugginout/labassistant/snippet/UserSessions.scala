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
  def dispatch : DispatchPF = {
    case Req(List("logout"), _, _) =>    
      session(Empty)
      S.redirectTo("/")
  }

  def snippetHandlers : SnippetPF = {
    case List("admin-required") => kickIfNotAdmin _
    case List("login-required") => kickIfNotLogged _
    case List("instructor-only") => showIfInstructor _
    case List("admin-only") => showIfAdmin _
    case List("student-only") => showIfStudent _
    case List("logged-in") => showIfLoggedIn _
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

  def showIfLoggedIn(xhtml:NodeSeq) : NodeSeq = {
    if (session.is.isDefined)
      xhtml
    else
      NodeSeq.Empty
  }

  def showIfStudent(xhtml:NodeSeq) : NodeSeq = {
    {
      for {
        session <- session.is
        user = session.user if user.student_?
      } yield {
        xhtml
      }
    } openOr
      NodeSeq.Empty
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

        //TODO maybe implement a cookie for auto reload of usersessions
        /*val sessionCookie =
          HTTPCookie("debuggin-lab-ass", session.uniqueId).
            setMaxAge((((session.expiresAt:TimeSpan) - millis) / 1000L).toInt).
            setPath("/")
        S.addCookie(sessionCookie)
        */
        session
      }
    printBox(userSession)

    session(userSession)
    printBox(session.is)

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
      case Some(user) if ! user.admin_? =>
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

      user match {
        // good login
        case Full(user) =>
          val actions =
            UserSessions.logUserIn(Full(user),
                                   GoHome)

          actions match {
            case Full(actions) => actions
            case Failure(message, _, _) => ShowError(message)
            case Empty => throw new Exception("something vewy vewy bad has happened.")
          }
        // bad login. check to suspend.
        case Failure(message, _, _) =>
          val numberOfFails = UserSessions.userFailedLogin(username)
          numberOfFails match {
            case Full(numFails) =>
              val response = {
                if (numFails == UserSession.SUSPEND_THRESHOLD)
                  "You have failed to login " + numFails + " consecutive times.\nYour account has been temporarily suspended."
                else
                  message + "\nYou have failed to login " + numFails + " consecutive times."              }
              ShowError(response)
            case _ =>
              ShowError(message)
          }
        case Empty =>
          ShowError("Shouldn't really be empty here... You found the matrix.")
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

