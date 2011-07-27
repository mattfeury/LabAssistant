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

object currentLab extends RequestVar[Box[Lab]](Empty)

object Labs {
	def rewriteRules : RewritePF = {
		case RewriteRequest(ParsePath("labs" :: labId :: Nil, _, _, _), _, _) =>
			//currentLab(Lab.find("uniqueId" -> labId))
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
						course = Some(course),
						teamSize = Integer.parseInt(teamSize), 
						role = role)
				//lab.save

        var message = "Generated."

        lab.role match {
          case Lab.Role.INDIVIDUAL =>
            lab.generateIndividualTeams
          case Lab.Role.RANDOM =>
            val leftovers = lab.generateRandomTeams
            if (leftovers > 0)
              message += " Warning: "+leftovers+" students could not be placed on a team. try a different size."
          case _ =>
        }

        Alert(message)

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
	lazy val lab = currentLab.is.open_!
	lazy val user = session.is.get.user
	
	def renderLabs = {
		val allLabs = List()//Lab.findAll(List())
		".lab" #> allLabs.map(Renderers.renderLab(_))
	}
	
	def renderLab(lab : Lab) = {

    def renderCreateTeam = {
      var name = ""

      def createTeam = {
        if (lab.studentIsOnTeam_?(user))
          Alert("student already on team")
        else {
          var team = Team(name, 0, Some(lab), List(user._id))
          //team.save
          team.teamWithNumber

          Alert("booyah")
        }
      }

      val processContents =
        ".name" #> text("", (value) => name = value.trim) &
        ".submit-button" #> onSubmitButtonLoginless(createTeam _)

      "*" #> { contents:NodeSeq =>
        ajaxForm(
          processContents(contents)
        )
      }

    }

    def generateTeams = {
      var message = "Generated."

      lab.role match {
        case Lab.Role.INDIVIDUAL =>
          lab.generateIndividualTeams
        case Lab.Role.RANDOM =>
          val leftovers = lab.generateRandomTeams
    			if (leftovers > 0)
            message += " Warning: "+leftovers+" students could not be placed on a team. try a different size."
        case _ =>
      }

      Alert(message)
    }

		".lab" #> Renderers.renderLab(lab) andThen
    ".teams" #> (
  		".team" #> lab.teams.map(Renderers.renderTeam(_))
    ) &
    ".student-panel" #> (lab.userIsStudent_?(user) ? PassThru | ClearNodes) andThen
    ".instructor-panel" #> (lab.userIsInstructor_?(user) ? PassThru | ClearNodes) andThen
    ".generate" #> (lab.isSelfSelect_? ? ClearNodes | PassThru) andThen
    ".generate" #> ajaxButton(Text("Generate Teams"), generateTeams _) &
    ".form" #> (lab.isSelfSelect_? ? PassThru | ClearNodes) andThen
    ".form" #> renderCreateTeam
	}
}
