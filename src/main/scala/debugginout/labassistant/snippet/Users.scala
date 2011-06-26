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
    case RewriteRequest(ParsePath("admin" :: "create" :: Nil, _, _, _), _, _) =>
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
    var firstName = ""
    var lastName = ""
    var role = S.attr("role")
      printBox(role)

    def validateSignup = {
      val user = 
        User(_id = username.toLowerCase, 
            username = username,
            email = formEmail.toLowerCase,
            password = User.cryptedPassword(password),
            firstName = firstName,
            lastName = lastName,
            role = role)

      user.save

      Alert("you win")
    }

    val processContents =
      ".username" #> text("", (value) => username = value.trim) &
      ".email" #> SHtml.email(formEmail, (value:String) => formEmail = value.trim) &
      ".password" #> SHtml.password("", (value) => password = value.trim) &
      ".first-name" #> text("", (value) => firstName = value.trim) &
      ".last-name" #> text("", (value) => lastName = value.trim) &
      ".submit-button" #> onSubmitButtonLoginless(validateSignup _)

    "*" #> { contents:NodeSeq =>
      ajaxForm(
        processContents(contents)
      )
    }


  }
}

