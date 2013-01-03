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

class AddLocalization {
  def render = {
    var label = ""
    var value = ""
    var languages = Language.findAll().map {
      lan =>
        (lan.name.is, lan.name.is)
    }
    var language = ""

    "#label" #> SHtml.text("", label = _) &
      "#text" #> SHtml.textarea("", value = _) &
      "#languages" #> SHtml.select(languages, Empty, language = _, "id" -> "languages") &
      "#addButton" #> SHtml.button(Text("Submit"), () => {
        Language.findByName(language).map {
          l =>
            Localization.add(label, value, l)
        }
        S.redirectTo(Site.crudLocalization.url)

      }) &
      "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudLocalization.url)

      })
  }
}