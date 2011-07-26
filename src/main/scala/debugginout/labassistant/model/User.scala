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

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._


case class UserSession(userId:String, ip:Option[String], valid:Boolean,
                      createdAt:Date = now, _id:ObjectId = ObjectId.get)
    extends MegaProtoUser[UserSession] {
  def meta = UserSession
  
  def user = {
    User.find("_id" -> userId).get
  }
}

object UserSession extends MegaProtoUserMeta[UserSession] {
  override def formats = allFormats

  // number of failed attempts before we suspend
  val SUSPEND_THRESHOLD = 3

  def forUser(userId:String) : List[UserSession] = {
    for {
      sessions <- findAll("userId" -> userId)
    } yield {
      sessions
    }
  }  
}



/**
 * An O-R mapped "User" class. Users should be created with a crypted password passed in.
 *
 * _id might be email ?
 */
case class User(_id:String, email:String, password:String, name:String,
              role:String = User.Role.STUDENT,
              status:Option[String] = None,
              createdAt:Option[Date] = Some(new Date)) extends MegaProtoUser[User] {
  def meta = User

  def courses = {
    Course.findAll("studentIds" -> _id)
  }

  lazy val admin_? = role == User.Role.ADMIN
  lazy val student_? = admin_? || role == User.Role.STUDENT
  lazy val instructor_? = admin_? || role == User.Role.INSTRUCTOR
  lazy val suspended_? = status == User.Status.SUSPENDED

  def is_?(userRole:String) = {
    role == userRole
  }
}

/**
 * The singleton that has methods for accessing the database
 */
object User extends MegaProtoUserMeta[User] {

  object Status {
    val SUSPENDED = "suspended"
  }

  object Role {
    val STUDENT = "student"
    val INSTRUCTOR = "instructor"
    val ADMIN = "admin"
  }
  
  def forEmail(email:String) : Box[User] = {
    for {
      user <- find("email" -> email)
    } yield {
      user
    }
  }

  def withLogin(idOrEmail:String) : Box[User] = {
    find(
      ("$or" ->
        List("_id" -> idOrEmail.toLowerCase,
             "email" -> idOrEmail.toLowerCase)))
  }

  def cryptedPassword(clearText:String) = {
    BCrypt.hashpw(clearText, BCrypt.gensalt())
  }

  def checkPassword(toCheck:String, hashedOriginal:String) = {
    BCrypt.checkpw(toCheck, hashedOriginal)
  }

  def forLogin(usernameOrEmail:String, password:String) : Box[User] = {
    for {
      user <- withLogin(usernameOrEmail) if checkPassword(password, user.password)
    } yield {
      user
    }
  }

}

