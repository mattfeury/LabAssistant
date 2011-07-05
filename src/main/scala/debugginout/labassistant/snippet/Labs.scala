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

object Labs {
	def rewriteRules : RewritePF = {
		case RewriteRequest(ParsePath("labs" :: Nil, _, _, _), _, _) =>
			  RewriteResponse("labs" :: "view" :: Nil, true) 
	}

	def createLabForm(course:Course) = {
		var name = ""
		var startTime = ""
		var endTime = ""
		var teamSize = ""
		var courseId = course.uniqueId
		var role = Lab.Role.RANDOM

		def createLab = {
			{
				for {
					session <- session.is
					user = session.user if user.instructor_? && course.userIsInstructor_?(user)
				} yield {
		
				val lab = 
					Lab(name = name,
						startTime = startTime, 
						endTime = endTime, 
						courseId = courseId,
						teamSize = Integer.parseInt(teamSize), 
						role = role)
				lab.save

				Alert("created")
				}
			} openOr
			Alert("failure")
		}
		
		val selectOpts = List((Lab.Role.RANDOM, "Random"),(Lab.Role.INDIVIDUAL, "Individual"), (Lab.Role.SELFSELECT, "Self Select"))


		val processContents =
		".name" #> text("", (value) => name = value.trim) &
		".role" #> select(selectOpts, Empty, (value) => role = value) &
		".teamSize" #> text("", (value) => teamSize = value.trim) &
		".startTime" #> text("", (value) => startTime = value.trim) &
		".endTime" #> text("", (value) => endTime = value.trim) &
		".submit-button" #> onSubmitButtonLoginless(createLab _)

		"*" #> { contents:NodeSeq =>
			ajaxForm(
				processContents(contents)
			)
		}
  }  
}

class Labs {
	def renderLabs = {
		val allLabs = Lab.findAll(List())

		".lab" #> allLabs.map(Renderers.renderLab(_))
	}



	
}
