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

class EditLanguage {
  def render(in: NodeSeq): NodeSeq = {
    import SHtml._
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
      language <- Site.editLanguageLoc.currentValue
    } yield {
      var title = language.name.is
      var check = language.isDefault.is
      out = (
        "#name" #> text(title, title = _) &
        "#checkDefault" #> checkbox(check, b => {
          check = b
        }) &
        "#addButton" #> button(Text("Submit"), () => {
          language.edit(title, check)
          S.redirectTo(Site.crudLanguage.url)
        }) &
        "#cancelButton" #> ajaxButton(Text("Cancel"), () => {
          S.redirectTo(Site.crudLanguage.url)

        })).apply(in)
    }

    out
  }
}