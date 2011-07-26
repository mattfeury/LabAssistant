/**
 * JsCmds for our models.
 */
package debugginout.labassistant
package snippet

import scala.xml._

import net.liftweb._
  import common._
  import http._
    import js._
      import JsCmds._    
      import JE._
  import json._
    import Extraction.decompose
    import JsonDSL._
    import JsCmds._
  import mongodb._
  import util._
    import Helpers._


/**
 * Passes through an HTML string unescaped. Wrap HTML from fixHtmlFunc in
 * this before passing it to Call or any other function that expects a JsExp,
 * otherwise your string will be double-escaped and things will break!
 */
case class Html(htmlString:String) extends JsExp {
  override val toJsCmd = htmlString
}

/**
 * basic javascript classes for sending xml
 */
class XmlFunctionCall(function:String, xml:NodeSeq, otherParams:JsExp*) extends JsCmd {
  override val toJsCmd = {
    fixHtmlFunc(function, xml) { html =>
      Call(function, (Html(html) :: otherParams.toList):_*).toJsCmd
    }
  }
}

class IdXmlFunctionCall(function:String, id:String, xml:NodeSeq, otherParams:JsExp*) extends JsCmd {
  override val toJsCmd = {
    fixHtmlFunc(function + id, xml) { html =>
      Call(function, (Str(id) :: Html(html) :: otherParams.toList):_*).toJsCmd
    }
  }
}

/**
 * General alerts, and what nots
 */
case class ShowError(error:String) extends JsCmd {
  override val toJsCmd = Call("showError", error).toJsCmd
}
case class ShowMessage(message:String) extends JsCmd {
  //TODO make this pretty
  override val toJsCmd = Call("alert", message).toJsCmd
}
case object GoHome extends JsCmd {
  override val toJsCmd = RedirectTo("/home").toJsCmd
}
case object Refresh extends JsCmd {
  override val toJsCmd = Call("location.reload(true)").toJsCmd
}

/**
 * Easy classes for inserting and deleting data based on any model
 */
abstract class InsertModel(model:String, id:String, xhtml:NodeSeq) extends IdXmlFunctionCall("insert" + model, id, xhtml)
abstract class RemoveModel(model:String, id:String) extends JsCmd {
  override val toJsCmd = Call("remove" + model, id).toJsCmd
}

case class InsertCourse(id:String, xhtml:NodeSeq) extends InsertModel("Course", id, xhtml)
case class RemoveCourse(id:String) extends RemoveModel("Course", id)

case class InsertLab(id:String, xhtml:NodeSeq) extends InsertModel("Lab", id, xhtml)
case class RemoveLab(id:String) extends RemoveModel("Lab", id)

case class InsertTeam(id:String, xhtml:NodeSeq) extends InsertModel("Team", id, xhtml)
case class RemoveTeam(id:String) extends RemoveModel("Team", id)

