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

class Admin {
  def renderUsers = {
    def renderUser(user:User) = {
      ".name *" #> user.getName &
      ".userId *" #> user._id &
      ".role *" #> user.getRole
    }

    val allUsers = User.findAll(List())

    //".template" #> ClearNodes andThen
    ".user" #> allUsers.map(renderUser(_))
  }

  def renderCreateInstructorForm = {

  }
}

