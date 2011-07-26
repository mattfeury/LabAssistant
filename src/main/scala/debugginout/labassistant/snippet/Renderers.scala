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
      import Helpers._

  import java.util.Date
  import debugginout.labassistant.lib._

  import debugginout.labassistant.model._
  import LinkHelpers._
  
  /*
   * General renders for most of our models
   */
  object Renderers {
    def renderUser(user:User) = {
      ".name *" #> user.name &
      ".userId *" #> user._id &
      ".role *" #> user.role
    }
    
    def renderCourse(course:Course, renderAsUser:Option[User] = None) : (NodeSeq)=>NodeSeq = {
      lazy val user = (renderAsUser == None) ? session.is.get.user | renderAsUser.get

      /**
       * Join the course.
       */
      def joinCourse = {
        if (user.student_?) {
          Course.update("uniqueId" -> course.uniqueId, "$addToSet" -> ("studentIds" -> user._id))
          course.labs.map(_.generateTeams)
          val newCourse = course.copy(studentIds = course.studentIds ::: List(user._id))
          ShowMessage("Successfully joined.") &
          InsertCourse(newCourse.uniqueId, (renderCourse(newCourse, renderAsUser)(Courses.detailedTemplate)))
        } else {
          ShowError("You are not a student.")
        }
      }

      /**
       * Leave the course.
       */
      def leaveCourse = {
        if (user.student_?) {
          Course.update("uniqueId" -> course.uniqueId, "$pull" -> ("studentIds" -> user._id))
          course.labs.map(_.generateTeams)

          val newCourse = course.copy(studentIds = course.studentIds.filterNot(_ == user._id))
        
          ShowMessage("Successfully left.") &
          InsertCourse(newCourse.uniqueId, (renderCourse(newCourse, renderAsUser)(Courses.detailedTemplate)))
        } else {
          ShowError("failure")
        }
      }

      /**
       * Delete this current course. Admins or the instructor only
       */
      def deleteCourse(s:String) = {
        if (user.admin_? || course.userIsInstructor_?(user)) {
          Course.delete("uniqueId" -> course.uniqueId)
          RemoveCourse(course.uniqueId)
        } else {
          ShowError("Could not delete. Invalid credentials.")
        }
      }

      // only seen on the detailed course page
      def renderInstructorPanel = {
        if (course.userIsInstructor_?(user)) {
          ".instructor-panel" #> (
            ".create-lab" #> Labs.createLabForm(course)
          )
        } else {
          ".instructor-panel" #> ClearNodes
        }
      }

      renderInstructorPanel andThen
      ".course [class+]" #> (course.studentIds.contains(user._id) ? "joined" | "not-joined") &
      ".course [class+]" #> (course.userIsInstructor_?(user) ? "instructor" | "") &
      ".course [data-id]" #> course.uniqueId &
      ".info" #> (
        ".name *" #> <a href={pathForCourse(course)} >{course.name}</a> &
        ".instructor *" #> course.instructor &
        ".num-labs *" #> course.labs.length &
        ".num-students *" #> course.students.length
      ) andThen
      ".delete [onclick]" #> onEventIf("Really delete?", deleteCourse _) &
      ".join" #> ajaxButton(Text("join"), joinCourse _) &
      ".leave" #> ajaxButton(Text("leave"), leaveCourse _)
    }
	
    def renderLab(lab:Lab, renderAsUser:Option[User] = None) = {
      lazy val user = (renderAsUser == None) ? session.is.get.user | renderAsUser.get
      lazy val course = lab.course.get

      def deleteLab(s:String) = {
        if (lab.course.map(_.userIsInstructor_?(user)) getOrElse false) {
          Lab.delete("uniqueId" -> lab.uniqueId)
          RemoveLab(lab.uniqueId)
        } else {
          ShowError("Could not delete.")
        }        
      }

      ".lab [data-id]" #> lab.uniqueId &
      ".name *" #> <a href={pathForLab(lab)} >{lab.name}</a> &
      ".course *" #> <a href={pathForCourse(course)} >{course.name}</a> &
      ".role *" #> lab.role &
      ".teamSize *" #> lab.teamSize &
      ".courseId *" #> lab.courseId &
      ".startTime *" #> lab.startTime &
      ".endTime *" #> lab.endTime &
      ".delete [onclick]" #> onEventIf("Really delete?", deleteLab _)      
    }
	
    def renderTeam(team:Team) = {
      lazy val user = session.is.get.user
      lazy val lab = team.lab.get

      def joinTeam = {
        if (lab.studentIsInCourseFor_?(user) && ! lab.studentIsOnTeam_?(user) && ! team.isFull_?) {
          Team.update("uniqueId" -> team.uniqueId, "$addToSet" -> ("studentIds" -> user._id))
          Alert("team joined")
        } else {
          Alert("Cannot join.")
        }
      }

      def leaveTeam = {
        if (lab.studentIsOnTeam_?(user)) {
          Team.update("uniqueId" -> team.uniqueId, "$pull" -> ("studentIds" -> user._id))
          Alert("team left")
        } else {
          Alert("cannot leave")
        }
      }

      ".team [data-id]" #> team.uniqueId &
      ".name *" #> team.name &
      ".size *" #> team.size &
      ".member *" #> team.students.map(renderUser(_)) &
      ".number *" #> team.number &
      ".controls" #> (lab.isSelfSelect_? ? PassThru | ClearNodes) andThen
      ".join" #> ajaxButton(Text("join"), joinTeam _) &
      ".leave" #> ajaxButton(Text("leave"), leaveTeam _)
    }
  }

} }
