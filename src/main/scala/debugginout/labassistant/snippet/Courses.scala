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

object Courses {
  def rewriteRules : RewritePF = {
    case RewriteRequest(ParsePath("courses" :: Nil, _, _, _), _, _) =>
      RewriteResponse("courses" :: "view" :: Nil, true)
  }

}

class Courses {
  def renderCourses = {
    val allCourses = Course.findAll(List())

    ".course" #> allCourses.map(Renderers.renderCourse(_))
  }

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

          Alert("created")
        }
      } openOr
        Alert("failure")
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

