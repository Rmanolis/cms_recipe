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

class EditTypeOfMeasure {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      typeOfMeasure <- Site.editCourseLoc.currentValue
      language <- typeOfMeasure.language.obj
    } yield {
      var name = typeOfMeasure.name.is
      var languages = Language.findAll().map {
        lan =>
          (lan.name.is, lan.name.is)
      }
      var lang = language.name.is

      out = (
        "#name" #> SHtml.text(name, name = _) &
        "#languages" #> SHtml.select(languages, Full(lang), lang = _, "id" -> "languages") &
        "#addButton" #> SHtml.button(Text("Submit"), () => {
          Language.findByName(lang).map {
            l =>
              typeOfMeasure.edit(l, name)
          }
          S.redirectTo(Site.crudTypeOfMeasure.url)

        }) &
        "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudTypeOfMeasure.url)

        })).apply(in)
    }
    out
  }
}