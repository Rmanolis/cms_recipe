package code.snippet
import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import util._
import http._
import common._
import Helpers._
import mapper._

import js.JsCmds._
import code.model._
import code.lib._

class AddIngredient {
  def render = {
    var name = ""
    var languages = Language.findAll().map {
      lan =>
        (lan.name.is, lan.name.is)
    }
    var language = ""
    var foodTypes = FoodType.findAll().map {
      fd => (fd.name.is, fd.name.is)
    }

    var foodType = ""
    "#name" #> SHtml.text(name, name = _) &
      "#languages" #> SHtml.ajaxSelect(languages, Empty, s => {
        language = s
        for {
          lang <- Language.findByName(language)
        } yield {
          foodTypes = FoodType.findByLanguage(lang).map {
            fd => (fd.name.is, fd.name.is)
          }
        }

        ReplaceOptions("foodTypes", List(("", "")) ++ foodTypes, Full(""))

      }, "id" -> "languages") &
      "#foodTypes" #> SHtml.ajaxUntrustedSelect(foodTypes, Empty, foodType = _, "id" -> "foodTypes") &
      "#addButton" #> SHtml.button(Text("Submit"), () => {
        Language.findByName(language).map {
          l =>
            FoodType.findByName(foodType).map {
              ft =>
                Ingredient.add(l, ft, name)
            }
        }
        S.redirectTo(Site.crudIngredient.url)

      }) &
      "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudIngredient.url)

      })
  }

}

class EditIngredient {

  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      ingredient <- Site.editIngredientLoc.currentValue
      language <- ingredient.language.obj
      foodType <- ingredient.foodType.obj
    } yield {
      var name = ingredient.name.is
      var languages = Language.findAll().map {
        lan =>
          (lan.name.is, lan.name.is)
      }
      var lang = language.name.is

      var foodTypes = FoodType.findAll().map {
        fd => (fd.name.is, fd.name.is)
      }
      var ftname = foodType.name.is

      out = (
        "#name" #> SHtml.text(name, name = _) &
        "#languages" #> SHtml.ajaxSelect(languages, Full(lang), s => {

          for {
            lang <- Language.findByName(s)
          } yield {
            foodTypes = FoodType.findByLanguage(lang).map {
              fd => (fd.name.is, fd.name.is)
            }
          }

          ReplaceOptions("foodTypes", List(("", "")) ++ foodTypes, Full(""))

        }, "id" -> "languages") &
        "#foodTypes" #> SHtml.ajaxUntrustedSelect(foodTypes, Empty, ftname = _, "id" -> "foodTypes") &
        "#addButton" #> SHtml.button(Text("Submit"), () => {
          Language.findByName(lang).map {
            l =>
              FoodType.findByName(ftname).map {
                ft =>
                  ingredient.edit(l, ft, name)
              }
          }
          S.redirectTo(Site.crudIngredient.url)

        }) &
        "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudIngredient.url)

        })).apply(in)
    }
    out
  }

}

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
      <tr>
        <th> Name </th>
        <th> Language </th>
        <th> Food Type </th>
        <th> Edit </th>
        <th> Delete </th> 
      </tr> ++
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
    }</table>

  }
}
