package debugginout.labassistant
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb._
  import http._
    import js._
      import JsCmds._    
      import JE._
    import provider._
    import LiftRules._        
    import SHtml._

import java.util.Date
import code.lib._
import Helpers._

import debugginout.labassistant.model._

object session extends SessionVar[Box[UserSession]](Empty)

object UserSessions { 

  def snippetHandlers : SnippetPF = {
    case List("admin-only") => showIfAdmin _
  }

  def showIfAdmin(xhtml:NodeSeq) : NodeSeq = {
    if (! session.is.isDefined || ! session.is.open_!.user.admin_?)
      S.redirectTo("/")

    xhtml
  }
  /**
   * Logs the user in and returns a Box. If the login was successful, a Full
   * box with JsCmds to run client-side is returned; otherwise, a Failure is
   * returned.
   */
  def logUserIn(user:Box[User], postLoginAction:JsCmd) : Box[JsCmd] = {
    printBox(user)
    printYellow(S.getRequestHeader("X-Forwarded-For"))
    val userSession =
      for {
        user <- user
        ip = S.getRequestHeader("X-Forwarded-For")
        session = UserSession(user._id, ip, true)
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
      Alert("session created") &
      postLoginAction
    } or {
      Failure("Session creation failed.")
    }
  }

}

class UserSessions {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  // replace the contents of the element with id "time" with the date
  def howdy = "#time *" #> date.map(_.toString)

  def loginForm = {
    var username = ""
    var password = ""

    def validateLogin = {
      val defaultError = "Check your username and password."

      val user =
        (User.forLogin(username, password) ?~ defaultError)

      printBox(user)
      user match {
        case Full(user) =>
          val actions =
            UserSessions.logUserIn(Full(user),
                                   Alert("logged in"))

          actions match {
            case Full(actions) => actions
            case Failure(message, _, _) => Alert(message)
            case Empty => throw new Exception("Uh... Unexpected Empty there, son.")
          }
        case Failure(message, _, _) =>
          Alert(message)
        case Empty =>
          Alert("Check your username and password.")
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

