package debugginout.labassistant
package model

import java.util.Date

import scala.xml._

import net.liftweb._
  import common._
  import json._
    import Extraction.decompose
    import JsonDSL._
  import mongodb._
  import util._
    import Helpers._

import org.bson.types.ObjectId
import lib._

/*
 * Courses
 */
case class Course(name:String, instructor:String, 
                  var studentIds:List[String] = List(),
                  var labs:List[Lab] = List(),
                  createdAt:Date = now, _id:ObjectId = ObjectId.get,
                  uniqueId:String = randomString(32)) extends MongoDocument[Course] {
  def meta = Course
  
  def students = {
    User.findAll(("_id" -> ("$in" -> studentIds)))
  }

  def insertLab(lab:Lab) = {
    lab.course = Some(this)
    labs = labs ::: List(lab)
  }

  def userIsInstructor_?(user:User) : Boolean = userIsInstructor_?(user._id)
  def userIsInstructor_?(userId:String) : Boolean = userId == instructor

  def userIsStudent_?(user:User) : Boolean = userIsStudent_?(user._id)
  def userIsStudent_?(userId:String) : Boolean = studentIds.contains(userId)
  
}

object Course extends MongoDocumentMeta[Course] {
  override def formats = allFormats

}

/*
 * Teams
 */
case class Team(name:String, var number:Int,
                lab:Option[Lab],
                var studentIds:List[String] = List(),
                _id:ObjectId = ObjectId.get,
                uniqueId:String = randomString(32)) {
  def students = User.findAll("_id" -> ("$in" -> studentIds))
  def size = studentIds.length

  def isFull_? = {
    {
      for {
        lab <- lab
      } yield {
        studentIds.length >= lab.teamSize
      }
    } getOrElse false
  }

  def studentIsOnTeam_?(user:User) = {
    if (user.student_?)
      studentIds.contains(user._id)
    else
      false
  }

  def teamWithNumber = {
    for {
      lab <- lab
      teams = lab.teams
    } yield {
      val id = teams.length
      number = id
    }
  }

}


/*
 * Labs
 */
case class Lab(name:String, startTime:String, endTime:String, 
               teamSize:Int, var course:Option[Course],
               role:String = Lab.Role.RANDOM,
               _id:ObjectId = ObjectId.get,
               var teams:List[Team] = List(),
               uniqueId:String = randomString(32)) {

  def isSelfSelect_? = role == Lab.Role.SELFSELECT

  def userIsInstructor_?(user:User) = {
    course.map(_.userIsInstructor_?(user)) getOrElse false
  } 
  def userIsStudent_?(user:User) = {
    course.map(_.userIsStudent_?(user)) getOrElse false
  }

  def studentIsOnTeam_?(user:User) = {
    teams.exists(_.studentIsOnTeam_?(user))
  }

  def studentIsInCourseFor_?(user:User) = {
    course.map(_.studentIds.contains(user._id)) getOrElse false
  }

  def deleteExistingTeams = {
    //delete existing teams
    teams = List()
    //Team.delete("labId" -> uniqueId)
  }

  def generateIndividualTeams = {
    if (role == Lab.Role.INDIVIDUAL)
    {
      deleteExistingTeams

      //make each student a team
      var i = 0
      for {
        course <- course
        student <- course.studentIds
      } {
        val team = Team(name = student,
                        number = i,
                        lab = Some(this),
                        studentIds = List(student))
        teams = teams ::: List(team)
        i = i + 1
      }
    }
  }

  def generateRandomTeams : Int = {
    {
      for {
        realCourse <- course
        students = realCourse.studentIds
      } yield {
        deleteExistingTeams

        val numberOfStudents = students.length
        //val numberOfTeams = numberOfStudents / teamSize
        val numberOfExtraStudents = numberOfStudents % teamSize
        var shuffledStudents = scala.util.Random.shuffle(students)
        var teamNumber = 0
        var createdTeams:List[Team] = List()
        
        while (shuffledStudents.length >= teamSize) {
          teamNumber = teamNumber + 1
          val teamStudentIds = shuffledStudents.take(teamSize)
          shuffledStudents = shuffledStudents.drop(teamSize)
          
          val team = Team(teamNumber.toString, teamNumber, Some(this), teamStudentIds)
          createdTeams = createdTeams ::: List(team)

        }

        for {
          createdTeam <- createdTeams if shuffledStudents.length > 0
        } {
          val studentId = shuffledStudents.head
          shuffledStudents = shuffledStudents.drop(1)

          createdTeam.studentIds = createdTeam.studentIds ::: List(studentId)
        }

        shuffledStudents.length
      }
    } getOrElse 0
  }



}

object Lab {

  object Role {
    val INDIVIDUAL = "individual"
    val RANDOM = "random"
    val SELFSELECT = "self-select"
  }
}
