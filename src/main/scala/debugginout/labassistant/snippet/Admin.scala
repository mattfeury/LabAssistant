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

class Admin {

  def renderCourses = {
    def renderCourse(course:Course) = {
      ".name *" #> course.name &
      ".instructor *" #> course.instructor &
      ".num-labs *" #> course.labs.length &
      ".num-students *" #> course.students.length
    }

    val allCourses = Course.findAll(List())

    ".course" #> allCourses.map(renderCourse(_))
  }

  def renderUsers = {
    def renderUser(user:User) = {
      ".name *" #> user.name &
      ".userId *" #> user._id &
      ".role *" #> user.getRoleName
    }

    val allUsers = User.findAll(List())

    ".user" #> allUsers.map(renderUser(_))
  }
}

