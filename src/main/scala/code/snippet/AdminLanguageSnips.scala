package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb._
import util._
import http._
import common._
import Helpers._

import js.JsCmds._
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
            if(Language.findByName(title).isEmpty){
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

object CRUDLanguages {
  def render(in: NodeSeq): NodeSeq = {
    import SHtml._

    <table id="listLanguages"> {
      <tr>
        <th> Name </th>
        <th> Edit </th>
        <th> Delete </th>
      </tr> ++
        Language.findAll.map {
          language =>
            <tr> {
              <td> {language.name.is} </td> ++
                <td> { button(Text("Edit"), () => {
                  S.redirectTo(Site.editLanguageLoc.calcHref(language))

                }) }</td> ++
                <td> {button(Text("Delete"), () => {
                  language.delete_!
                 S.redirectTo(Site.crudLanguage.url)
                }) }</td>
            }</tr>
        }
    }</table>

  }
}