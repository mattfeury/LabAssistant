package debugginout.labassistant
package snippet 

import scala.xml._

import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.util._
  import Helpers._

object TemplateHelpers {
  def findXml(nodes:NodeSeq, cssSelector:String) : NodeSeq = {
    var xml:Box[NodeSeq] = Empty

    val cssSelectorParts = cssSelector.split(" ")
    val parents = cssSelectorParts.take(cssSelectorParts.length - 1)
    val deepestSelector = cssSelectorParts.takeRight(1)(0)

    val extractor : (NodeSeq)=>NodeSeq =
      deepestSelector #> { ns:NodeSeq => if (xml.isEmpty) xml = Full(ns); ns }
    val completeExtractor =
      parents.foldRight(extractor) { (selector, extractor) =>
        selector #> extractor
      }

    completeExtractor(nodes)

    xml openOr NodeSeq.Empty
  }

  /**
   * Looks up a template at the given templatePath and extracts the xml that
   * matches the given CSS selector. The selector should be made up only of
   * Lift-compatible selector expressions, and does not support child
   * selectors at the moment, only descendant selectors. Also note that the
   * selected XML is the result of the *first* match of the selector.
   *
   * Note that in the background, a selector like .update-content .replies .reply
   * will generate a CSS extractor like so:
   * ".update-content" #> (".replies" #> (".reply" #> { extract }))
   */
  def findXmlInTemplate(templatePath:List[String], cssSelector:String) : NodeSeq = {
    Templates(templatePath).map { template =>
      findXml(template, cssSelector)
    } openOr {
      NodeSeq.Empty
    }
  }

  /**
   * See findXmlInTemplate(List[String], String). Allows the template path to
   * be a /-delimited path instead of a List.
   */
  def findXmlInTemplate(templatePath:String, cssSelector:String) : NodeSeq = {
    findXmlInTemplate(templatePath.split("/").toList, cssSelector)
  }

}  

