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
  import Renderers._
  import TemplateHelpers._

object currentCourse extends RequestVar[Box[Course]](Empty)

object Courses {
  def rewriteRules : RewritePF = {
    case RewriteRequest(ParsePath("courses" :: courseId :: Nil, _, _, _), _, _) =>
      currentCourse(Course.find("uniqueId" -> courseId)) 
      RewriteResponse("courses" :: "view" :: Nil, true)
  }

  lazy val detailedTemplate = findXmlInTemplate("templates-hidden/course", ".course")
  lazy val liCourseTemplate = findXmlInTemplate("courses", ".course")
  lazy val liLabTemplate = findXmlInTemplate("templates-hidden/course", ".lab")  
}

class Courses {
  lazy val course = currentCourse.is.open_!
  lazy val user = session.is.get.user

  def renderAllCourses = {
    val allCourses = Course.findAll(List())

    ".course" #> allCourses.map(renderCourse(_))
  }
  
  def renderDetailedCourse = {
    renderInstructorPanel andThen
    ".course" #> renderCourse(course) andThen
		".lab" #> course.labs.map(renderLab(_))
  }

  // only seen on the detailed course page
  def renderInstructorPanel = {
    if (course.userIsInstructor_?(user)) {
      ".instructor-panel" #> (
        ".create-form" #> Labs.createLabForm(course)
      )
    } else {
      ".instructor-panel" #> ClearNodes
    }
  }

  //called from the home page for instructors
  def createCourseForm = {
    var name = ""

    def createCourse = {
      {
        for {
          session <- session.is
          user = session.user if user.instructor_?
        } yield {
          val course = Course(name, user._id)
          course.save

          InsertCourse(course.uniqueId, (Renderers.renderCourse(course, Full(user))(Courses.liCourseTemplate)))
        }
      } openOr
        ShowError("There was an error.")
    }

    val processContents =
      ".name" #> text("", (value) => name = value.trim) &
      ".submit-button" #> onSubmitButtonLoginless(createCourse _)

    "*" #> { contents:NodeSeq =>
      ajaxForm(
        processContents(contents)
      )
    }


  }
}

