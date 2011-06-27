package bootstrap.liftweb

import net.liftweb._
  import common._
  import json.JsonDSL._
  import net.liftweb.mongodb._
  import util.Props
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._

import com.mongodb.{Mongo, MongoOptions, ServerAddress}

import debugginout.labassistant._
  import model._
  import snippet._


object MongoConfig {
  def init: Unit = {
    val srvr = new ServerAddress(
       Props.get("mongo.host", "127.0.0.1"),
       Props.getInt("mongo.port", 27017)
    )

    val mongoHost = MongoHost(Props.get("mongo.host") openOr "127.0.0.1",
                               (for (port <- Props.get("mongo.port")) yield port.toInt) openOr 27017)
    MongoDB.defineDb(DefaultMongoIdentifier, MongoAddress(mongoHost, Props.get("mongo.db") openOr "debuggin"))

    //MongoDB.defineDb(DefaultMongoIdentifier, new Mongo(srvr), Props.get("mongo.db") openOr "debuggin")
  }
}
  
/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {

    MongoConfig.init

    printGreen("BOOTation")

    // snippet stuff
    LiftRules.addToPackages("debugginout.labassistant")
    LiftRules.statelessRewrite.append(Users.rewriteRules)
    LiftRules.statelessRewrite.append(Courses.rewriteRules)
    LiftRules.snippets.append(UserSessions.snippetHandlers)
    LiftRules.dispatch.append(UserSessions.dispatch)
//    LiftRules.dispatch.append(Users.dispatch)
        
    // Use jQuery 1.4
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)
    
    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))    
    
  }
}

