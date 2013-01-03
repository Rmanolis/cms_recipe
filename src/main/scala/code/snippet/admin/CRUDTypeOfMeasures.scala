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

class CRUDTypeOfMeasures extends PaginatorSnippet[TypeOfMeasure] {
  override def count = TypeOfMeasure.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[QueryParam[TypeOfMeasure]] = List()
    list +:= OrderBy(TypeOfMeasure.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    TypeOfMeasure.findAll(list: _*)
  }
  def renderPage(in: NodeSeq): NodeSeq = {
    import SHtml._

    <table id="listTypeOfMeasures"> {
      <thead>
        <tr>
          <th> Name </th>
          <th> Language </th>
          <th> Edit </th>
          <th> Delete </th>
        </tr>
      </thead> ++
        <tbody>{
          page.map {
            tom =>
              <tr> {
                <td> { tom.name.is } </td> ++
                  <td>{
                    tom.getLanguageName
                  } </td> ++
                  <td> {
                    button(Text("Edit"), () => {
                      S.redirectTo(Site.editTypeOfMeasureLoc.calcHref(tom))

                    })
                  }</td> ++
                  <td> {
                    button(Text("Delete"), () => {
                      tom.delete
                      S.redirectTo(Site.crudTypeOfMeasure.url)
                    })
                  }</td>
              }</tr>
          }
        }</tbody>
    }</table>

  }
}