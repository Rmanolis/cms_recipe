package code.snippet
import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import util._
import http._
import common._
import Helpers._

import js._
import js.JsCmds._
import js.JE._
import code.model._
import code.lib._

class GetLabel {

  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      lan <- LanguageHelper.getLanguage
      ll <- Localization.findByLabelAndLanguage(in.text.trim, lan)
    } yield {
    	out = ("#label" #> ll.text.is).apply(in)
    }
    out
  }
}