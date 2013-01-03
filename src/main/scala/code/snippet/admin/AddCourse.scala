package code.snippet.admin
import net.liftweb._
import common._
import util._
import http._
import scala.xml._
import Helpers._
import code.model._
import code.lib._

class AddCourse {
def render = {
    var name = ""
    var languages = Language.findAll().map {
      lan =>
        (lan.name.is, lan.name.is)
    }
    var language = ""
    "#name" #> SHtml.text(name, name = _) &
      "#languages" #> SHtml.select(languages, Empty, language = _, "id" -> "languages") &
      "#addButton" #> SHtml.button(Text("Submit"), () => {
        Language.findByName(language).map {
          l =>
            Course.add(l, name)
        }
        S.redirectTo(Site.crudCourse.url)

      }) &
      "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudCourse.url)

      })
  }
}