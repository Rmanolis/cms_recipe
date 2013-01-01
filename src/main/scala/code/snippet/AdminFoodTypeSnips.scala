package code.snippet

import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import util._
import http._
import common._
import mapper._
import Helpers._

import js.JsCmds._
import code.model._
import code.lib._

class AddFoodType {
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
            FoodType.add(l, name)
        }
        S.redirectTo(Site.crudFoodType.url)

      }) &
      "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudFoodType.url)

      })
  }
}

class EditFoodType {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      foodType <- Site.editFoodTypeLoc.currentValue
      language <- foodType.language.obj
    } yield {
      var name = foodType.name.is
      var languages = Language.findAll().map {
        lan =>
          (lan.name.is, lan.name.is)
      }
      var lang = language.name.is

      out = (
        "#name" #> SHtml.text(name, name = _) &
        "#languages" #> SHtml.select(languages, Full(lang), lang = _, "id" -> "languages") &
        "#addButton" #> SHtml.button(Text("Submit"), () => {
          Language.findByName(lang).map {
            l =>
              foodType.edit(l, name)
          }
          S.redirectTo(Site.crudFoodType.url)

        }) &
        "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudFoodType.url)

        })).apply(in)
    }
    out
  }

}

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
      <tr>
        <th> Name </th>
        <th> Language </th>
        <th> Edit </th>
        <th> Clean dependents</th>
    	<th> Delete dependents</th>
        <th> Delete </th>
      </tr> ++
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
    }</table>

  }
}