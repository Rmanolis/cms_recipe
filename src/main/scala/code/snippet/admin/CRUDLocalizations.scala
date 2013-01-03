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

class CRUDLocalizations extends PaginatorSnippet[Localization] {
  override def count = Localization.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[QueryParam[Localization]] = List()
    list +:= OrderBy(Localization.label, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    Localization.findAll(list: _*)
  }
  def renderPage(in: NodeSeq): NodeSeq = {
    <table id="listOfLocalesByLabel"> {
      <thead>
        <tr>
          <th> Label </th>
          <th> Text </th>
          <th> Language </th>
          <th> Edit </th>
          <th> Delete </th>
        </tr>
      </thead> ++
        <tbody>{
          page.map {
            ll =>
              <tr> {
                <td> { ll.label.is }</td> ++
                  <td> { ll.text.is }</td> ++
                  <td> { ll.getLanguageName } </td> ++
                  <td>{
                    SHtml.button(Text("Edit"), () => {
                      S.redirectTo(Site.editLocalizationLoc.calcHref(ll))

                    })
                  }</td> ++
                  <td> {
                    SHtml.button(Text("Delete"), () => {
                      ll.delete_!
                      S.redirectTo(Site.crudLocalization.url)

                    })
                  }</td>
              }</tr>
          }
        }</tbody>
    } </table>
  }

}