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
import lib.BCrypt

case class UserSession(userId:String, ip:Option[String],
                       valid:Boolean, createdAt:Date = now,
                       _id:ObjectId = ObjectId.get, uniqueId:String = randomString(32))
    extends MongoDocument[UserSession] {
  import http.TransientRequestVar

  def meta = UserSession

  private object reqUser extends TransientRequestVar[User](User.find("_id" -> userId).get)
  def user = {
    tryo(reqUser.get) openOr User.find("_id" -> userId).get
  }

  /*
  def expire_! = {
    copy(destroyedAt = Some(now)).save
  }*/
}

object UserSession extends MongoDocumentMeta[UserSession] {

}



/**
 * An O-R mapped "User" class. Users should be created with a crypted password passed in.
 *
 * _id might be email ?
 */
case class User(_id:String, username:String, 
              email:String, password:String, 
              role:Option[String] = None,
              createdAt:Option[Date] = Some(new Date)) extends MongoDocument[User] {
  def meta = User

  lazy val admin_? = role.map(_ == User.Role.ADMIN) getOrElse false
}

/**
 * The singleton that has methods for accessing the database
 */
object User extends MongoDocumentMeta[User] {

  object Role {
    val INSTRUCTOR = "instructor"
    val ADMIN = "admin"
  }
  
  def forEmail(email:String) : Box[User] = {
    for {
      user <- User.find("email" -> email)
    } yield {
      user
    }
  }

  def withLogin(usernameOrEmail:String) : Box[User] = {
    find(
      ("$or" ->
        List("_id" -> usernameOrEmail.toLowerCase,
             "email" -> usernameOrEmail.toLowerCase)))
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

