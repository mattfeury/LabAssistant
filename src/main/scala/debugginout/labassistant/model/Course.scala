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
                  labIds:List[String] = List(), studentIds:List[String] = List(),
                  createdAt:Date = now, _id:ObjectId = ObjectId.get,
                  uniqueId:String = randomString(32)) extends MongoDocument[Course] {
  def meta = Course
  
  def students = {
    User.findAll(("_id" -> ("$in" -> studentIds)))
  }

  def labs = {
    Lab.findAll(("uniqueId" -> ("$in" -> labIds)))
  }

  def userIsInstructor_?(user:User) : Boolean = userIsInstructor_?(user._id)
  def userIsInstructor_?(userId:String) : Boolean = userId == instructor
  
}

object Course extends MongoDocumentMeta[Course] {
  override def formats = allFormats

}

/*
 * Teams
 */
case class Team(name:String, number:Int,
                studentIds:List[String] = List(),
                _id:ObjectId = ObjectId.get,
                uniqueId:String = randomString(32)) extends MongoDocument[Team] {
  def meta = Team
  
  def students = User.findAll("_id" -> ("$in" -> studentIds))
  def lab = Lab.find("teamIds" -> ("$in" -> uniqueId))
}

object Team extends MongoDocumentMeta[Team] {
  override def formats = allFormats
}


/*
 * Labs
 */
case class Lab(name:String, startTime:String, endTime:String, 
               teamSize:Int, courseId:String,
               role:String = Lab.Role.RANDOM,
               teamIds:List[String] = List(),               
               _id:ObjectId = ObjectId.get,
               uniqueId:String = randomString(32)) extends MongoDocument[Lab] {
  def meta = Lab
  
  def course = Course.find("uniqueId" -> courseId)
  def teams = Team.findAll("uniqueId" -> ("$in" -> teamIds))

  def generateRandomTeams = {
    for {
      realCourse <- course
      students = realCourse.studentIds
    } {
      val numberOfStudents = students.length
      //val numberOfTeams = numberOfStudents / teamSize
      val numberOfExtraStudents = numberOfStudents % teamSize
      var shuffledStudents = scala.util.Random.shuffle(students)
      var teamNumber = 0
      var createdTeamIds:List[String] = List()
      
      while (shuffledStudents.length >= teamSize) {
        teamNumber = teamNumber + 1
        val teamStudentIds = shuffledStudents.take(teamSize)
        shuffledStudents = shuffledStudents.drop(teamSize)
        
        val team = Team(teamNumber.toString, teamNumber, teamStudentIds)
        team.save
        createdTeamIds = createdTeamIds ::: List(team.uniqueId)

      }
      for {
        createdTeamId <- createdTeamIds if shuffledStudents.length > 0
      } {
        val studentId = shuffledStudents.take(1)
        shuffledStudents = shuffledStudents.drop(1)

        Team.update("uniqueId" -> createdTeamId, ("$addToSet" -> ("studentIds" -> studentId)))
      }      
      
      
    }
  }



}

object Lab extends MongoDocumentMeta[Lab] {
  override def formats = allFormats

  object Role {
    val INDIVIDUAL = "individual"
    val RANDOM = "random"
    val SELFSELECT = "self-select"
  }
}
