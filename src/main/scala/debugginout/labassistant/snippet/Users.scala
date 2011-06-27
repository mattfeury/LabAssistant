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
import debugginout.labassistant.lib._
import Helpers._

import debugginout.labassistant.model._

object Users {

  def rewriteRules : RewritePF = {
    case RewriteRequest(ParsePath("settings" :: Nil, _, _, _), _, _) =>
      RewriteResponse("users" :: "edit" :: Nil, true)
    
    case RewriteRequest(ParsePath("users" :: "create" :: Nil, _, _, _), _, _) =>
      RewriteResponse("users" :: "create" :: Nil, true)
  }
  
  /*def dispatch : DispatchPF = {
    case Req(List("create"), _, _) =>
      {
        for {
          session <- session.is
          user <- session.user if user.admin_?
        } yield {
          true
        }
      } getOrElse
        S.redirectTo("/")

  }*/
}

class Users {

  def createUserForm = {
    var username = ""
    var formEmail = ""
    var password = ""
    var name = ""
    var role:Option[String] = Empty
    val preferredRole = S.attr("role")

    def validateSignup = {

      var properRole:Option[String] = (preferredRole orElse role) match {
        case Some("") =>
          Empty
        case m @ _ =>
          m
      }

      val user = 
        User(_id = username.toLowerCase, 
            username = username,
            email = formEmail.toLowerCase,
            password = User.cryptedPassword(password),
            name = name,
            role = properRole)

      user.save

      Alert("you win")
    }

    val selectOpts = List(("", "Student"),(User.Role.INSTRUCTOR, "Instructor"), (User.Role.ADMIN, "Admin"))

    val processContents =
      ".username" #> text("", (value) => username = value.trim) &
      ".email" #> email(formEmail, (value:String) => formEmail = value.trim) &
      ".password" #> SHtml.password("", (value) => password = value.trim) &
      ".name" #> text("", (value) => name = value.trim) &
      ".role" #> select(selectOpts, Empty, (value) => role = Some(value)) &
      ".submit-button" #> onSubmitButtonLoginless(validateSignup _)

    "*" #> { contents:NodeSeq =>
      ajaxForm(
        processContents(contents)
      )
    }


  }
}

