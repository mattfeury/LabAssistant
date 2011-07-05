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
  
  object LinkHelpers{
    
    def pathForCourse(course:Course) : String = pathForCourse(course.uniqueId)
    def pathForCourse(courseId:String) : String = "/courses/" + courseId
    
    def pathForLab(lab:Lab) : String = pathForLab(lab.uniqueId)
    def pathForLab(labId:String) : String = "/labs/" + labId
    
    def pathForUser(user:User) : String = pathForUser(user._id)
    def pathForUser(userId:String) : String = "/users/" + userId
    
    def pathForTeam(team:Team) : String = pathForTeam(team.uniqueId)
    def pathForTeam(teamId:String) : String = "/teams/" + teamId
  }
  
  } 
}
