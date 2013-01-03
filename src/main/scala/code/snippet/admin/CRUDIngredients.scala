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

class CRUDIngredients extends PaginatorSnippet[Ingredient] {
  override def count = Ingredient.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[QueryParam[Ingredient]] = List()
    list +:= OrderBy(Ingredient.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    Ingredient.findAll(list: _*)
  }
  def renderPage(in: NodeSeq): NodeSeq = {
    import SHtml._

    <table id="listIngredients"> {
      <thead>
        <tr>
          <th> Name </th>
          <th> Language </th>
          <th> Food Type </th>
          <th> Edit </th>
          <th> Delete </th>
        </tr>
      </thead> ++
        <tbody>{
          page.map {
            ingredient =>
              <tr> {
                <td> { ingredient.name.is } </td> ++
                  <td>{
                    ingredient.getLanguageName
                  } </td> ++
                  <td>{
                    ingredient.getFoodTypeName
                  } </td> ++
                  <td> {
                    button(Text("Edit"), () => {
                      S.redirectTo(Site.editIngredientLoc.calcHref(ingredient))

                    })
                  }</td> ++
                  <td> {
                    button(Text("Delete"), () => {
                      ingredient.delete
                      S.redirectTo(Site.crudIngredient.url)
                    })
                  }</td>
              }</tr>
          }
        }</tbody>
    }</table>

  }
}