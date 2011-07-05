package debugginout.labassistant { package snippet {

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

  /*
   * General renders for most of our models
   */
  object Renderers {
    def renderUser(user:User) = {
      ".name *" #> user.name &
      ".userId *" #> user._id &
      ".role *" #> user.getRoleName
    }
    
    def renderCourse(course:Course, renderAsUser:Option[User] = None) = {
      lazy val user = (renderAsUser == None) ? session.is.get.user | renderAsUser.get

      def joinCourse = {
        if (user.student_?) {
          Course.update("uniqueId" -> course.uniqueId, "$addToSet" -> ("studentIds" -> user._id))
          Alert("course joined")
        } else {
          Alert("You are not a student.")
        }
      }

      def leaveCourse = {
        if (user.student_?) {
          Course.update("uniqueId" -> course.uniqueId, "$pull" -> ("studentIds" -> user._id))
          Alert("course left")
        } else {
          Alert("failure")
        }
      }

      ".course [class+]" #> (course.studentIds.contains(user._id) ? "joined" | "not-joined") &
      ".course [class+]" #> (course.userIsInstructor_?(user) ? "instructor" | "") &
      ".name *" #> <a href={"/courses/"+course.uniqueId} >{course.name}</a> &
      ".instructor *" #> course.instructor &
      ".num-labs *" #> course.labs.length &
      ".num-students *" #> course.students.length &
      ".join" #> ajaxButton(Text("join"), joinCourse _) &
      ".leave" #> ajaxButton(Text("leave"), leaveCourse _)
    }
	
	def renderLab(lab:Lab) = {
		".name *" #> lab.name &
		".role *" #> lab.role &
		".teamSize *" #> lab.teamSize &
		".courseId *" #> lab.courseId &
		".startTime *" #> lab.startTime &
		".endTime *" #> lab.endTime
    }
  }
} }
