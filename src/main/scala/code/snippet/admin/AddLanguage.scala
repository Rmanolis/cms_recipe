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

class AddLanguage {
  def render(in: NodeSeq): NodeSeq = {
    import SHtml._
    var title = ""
    var check = false
    var out = NodeSeq.Empty
    User.currentUser.map {
      x =>
        out = (
          "#name" #> text(title, title = _) &
          "#checkDefault" #> checkbox(check, b => {
            check = b
          }) &
          "#addButton" #> button(Text("Submit"), () => {
            if (Language.findByName(title).isEmpty) {
              Language.add(title, check)
            }
            S.redirectTo(Site.crudLanguage.url)

          }) &
          "#cancelButton" #> button(Text("Cancel"), () => {
            S.redirectTo(Site.crudLanguage.url)

          })).apply(in)
    }
    out
  }
}