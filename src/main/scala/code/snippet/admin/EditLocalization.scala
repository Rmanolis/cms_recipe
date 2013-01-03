package code.snippet.admin
import net.liftweb._
import common._
import util._
import http._
import mapper._
import scala.xml._
import Helpers._
import code.model._
import code.lib._

class EditLocalization {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      ll <- Site.editLocalizationLoc.currentValue
      language <- ll.language.obj
    } yield {
      val languages = Language.findAll.map(l => (l.id.is.toString, l.name.is))
      var label = ll.label.is
      var text = ll.text.is
      var lan = language.name.is
      out = ("#label" #> SHtml.text(label, label = _) &
        "#text" #> SHtml.textarea(text, text = _) &
        "#languages" #> SHtml.select(languages, Full(lan), lan = _, "id" -> "languages") &
        "#addButton" #> SHtml.button(Text("Submit"), () => {
          Language.findByName(lan).map {
            l =>
              ll.edit(label, text, l)
          }
          S.redirectTo(Site.crudLocalization.url)

        }) &
        "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudLocalization.url)

        })).apply(in)
    }
    out
  }
}