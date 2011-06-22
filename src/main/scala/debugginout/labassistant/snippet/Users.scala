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
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  // replace the contents of the element with id "time" with the date
  def howdy = "#time *" #> date.map(_.toString)

  def createUserForm = {
    var username = ""
    var formEmail = ""
    var password = ""

    def validateSignup = {
      println(username)
        val user = 
          User(_id = username.toLowerCase, username = username,
               email = formEmail.toLowerCase,
               password = password)

        user.save

        Alert("you win")

    }

    val processContents =
      "#username" #> text("", (value) => username = value.trim) &
      "#email" #> SHtml.email(formEmail, (value:String) => formEmail = value.trim) &
      "#password" #> SHtml.password("", (value) => password = value.trim) &
      ".submit-button" #> onSubmitButtonLoginless(validateSignup _)

    "*" #> { contents:NodeSeq =>
      ajaxForm(
        processContents(contents)
      )
    }


  }
}

