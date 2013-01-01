package code.snippet

import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import util._
import http._
import common._
import Helpers._

import js.JsCmds._
import code.model._
import code.lib._
import net.liftweb.mapper._

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

class CRUDLanguages extends PaginatorSnippet[Language] {
  override def count = Language.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[QueryParam[Language]] = List()
    list +:= OrderBy(Language.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    Language.findAll(list: _*)
  }
  def renderPage(in: NodeSeq): NodeSeq = {
    import SHtml._

    <table id="listLanguages"> {
      <tr>
        <th> Name </th>
        <th> Edit </th>
        <th> Clean Dependents </th>
        <th> Delete Dependents </th>
        <th> Delete  </th>
      </tr> ++
        page.map {
          language =>
            <tr> {
              <td> { language.name.is } </td> ++
                <td> {
                  button(Text("Edit"), () => {
                    S.redirectTo(Site.editLanguageLoc.calcHref(language))

                  })
                }</td> ++
                <td> {
                  button(Text("Clean Dependents"), () => {
                    language.cleanDependents
                    S.redirectTo(Site.crudLanguage.url)
                  })
                }</td> ++
                <td> {
                  button(Text("Delete Dependents"), () => {
                    language.deleteDependents
                    S.redirectTo(Site.crudLanguage.url)
                  })
                }</td> ++
                <td> {
                  button(Text("Delete"), () => {
                    language.cleanDependents
                    language.delete_!
                    S.redirectTo(Site.crudLanguage.url)
                  })
                }</td>
            }</tr>
        }
    }</table>

  }
}