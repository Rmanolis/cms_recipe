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
      <thead>
        <tr>
          <th> Name </th>
          <th> Edit </th>
          <th> Clean Dependents </th>
          <th> Delete Dependents </th>
          <th> Delete  </th>
        </tr>
      </thead> ++
        <tbody>{
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
        }</tbody>
    }</table>

  }
}