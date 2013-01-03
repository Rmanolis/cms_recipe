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

class CRUDFoodTypes extends PaginatorSnippet[FoodType] {
  override def count = FoodType.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[QueryParam[FoodType]] = List()
    list +:= OrderBy(FoodType.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    FoodType.findAll(list: _*)
  }
  def renderPage(in: NodeSeq): NodeSeq = {
    import SHtml._

    <table id="listFoodTypes"> {
      <thead>
        <tr>
          <th> Name </th>
          <th> Language </th>
          <th> Edit </th>
          <th> Clean dependents</th>
          <th> Delete dependents</th>
          <th> Delete </th>
        </tr>
      </thead> ++
        <tbody>{
          page.map {
            foodType =>
              <tr> {
                <td> { foodType.name.is } </td> ++
                  <td>{
                    foodType.getLanguageName
                  } </td> ++
                  <td> {
                    button(Text("Edit"), () => {
                      S.redirectTo(Site.editFoodTypeLoc.calcHref(foodType))

                    })
                  }</td> ++
                  <td> {
                    button(Text("Clean dependents"), () => {
                      foodType.cleanDependents
                      S.redirectTo(Site.crudFoodType.url)
                    })
                  }</td> ++
                  <td> {
                    button(Text("Edit"), () => {
                      foodType.deleteDependents
                      S.redirectTo(Site.editFoodTypeLoc.calcHref(foodType))

                    })
                  }</td> ++
                  <td> {
                    button(Text("Delete"), () => {
                      foodType.cleanDependents
                      foodType.delete_!
                      S.redirectTo(Site.crudFoodType.url)
                    })
                  }</td>
              }</tr>
          }
        }</tbody>
    }</table>

  }
}