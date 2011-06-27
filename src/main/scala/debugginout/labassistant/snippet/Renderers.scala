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
    
    def renderCourse(course:Course) = {

      def joinCourse = {
        {
          for {
            session <- session.is
            user = session.user if user.student_?
          } yield {
            Course.update("uniqueId" -> course.uniqueId, "$addToSet" -> ("studentIds" -> user._id))
            Alert("course joined")
          }
        } openOr
          Alert("failure")

      }

      def leaveCourse = {
        {
          for {
            session <- session.is
            user = session.user if user.student_?
          } yield {
            Course.update("uniqueId" -> course.uniqueId, "$pullAll" -> ("studentIds" -> user._id))
            Alert("course left")
          }
        } openOr
          Alert("failure")

      }

      ".name *" #> course.name &
      ".instructor *" #> course.instructor &
      ".num-labs *" #> course.labs.length &
      ".num-students *" #> course.students.length &
      ".join" #> a(joinCourse _, Text("join")) &
      ".leave" #> a(leaveCourse _, Text("leave"))
    }
  }
} }
