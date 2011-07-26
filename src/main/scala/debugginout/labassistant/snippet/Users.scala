package debugginout.labassistant
package snippet 

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb._
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
import debugginout.labassistant.snippet._

object Users {
  def rewriteRules : RewritePF = {
    case RewriteRequest(ParsePath("settings" :: Nil, _, _, _), _, _) =>
      RewriteResponse("users" :: "edit" :: Nil, true)
    
    case RewriteRequest(ParsePath("users" :: "create" :: Nil, _, _, _), _, _) =>
      RewriteResponse("users" :: "create" :: Nil, true)
  }
  
}

class Users {
  lazy val user = session.is.get.user

  def renderCourses = {
    val coursesByRole = user.role match {
      case User.Role.INSTRUCTOR =>
        Course.findAll("instructor" -> user._id)
      case User.Role.ADMIN =>
        user.courses ::: Course.findAll("instructor" -> user._id)
      case _ =>
        user.courses   
    }
    
    ".course" #> coursesByRole.distinct.map(Renderers.renderCourse(_, Some(user)))
  }

  def renderUsers = {
    val allUsers = User.findAll(List())
    ".user" #> allUsers.map(Renderers.renderUser(_))
  }  

  /**
   * Render the create user form from the snippet template.
   * This will validate form input and submit handlers
   */
  def createUserForm = {
    var username = ""
    var formEmail = ""
    var password = ""
    var name = ""
    var role = User.Role.STUDENT
    val preferredRole = S.attr("role")
    val shouldLogin = S.attr("login").isDefined

    def validateSignup = {
      val properRole = preferredRole getOrElse role

      val user = 
        User(_id = username.toLowerCase, 
            email = formEmail.toLowerCase,
            password = User.cryptedPassword(password),
            name = name,
            role = properRole)

      user.save

      if (shouldLogin)
        UserSessions.logUserIn(Full(user), GoHome).getOrElse(GoHome)
      else
        ShowMessage("Success.") //should maybe insert student.
    }

    val selectOpts = List((User.Role.STUDENT, "Student"),(User.Role.INSTRUCTOR, "Instructor"), (User.Role.ADMIN, "Admin"))

    val processContents =
      ".username" #> text("", (value) => username = value.trim) &
      ".email" #> email(formEmail, (value:String) => formEmail = value.trim) &
      ".password" #> SHtml.password("", (value) => password = value.trim) &
      ".name" #> text("", (value) => name = value.trim) &
      ".role" #> select(selectOpts, Empty, (value) => role = value) &
      ".submit-button" #> onSubmitButtonLoginless(validateSignup _)

    "*" #> { contents:NodeSeq =>
      ajaxForm(
        processContents(contents)
      )
    }


  }
}

