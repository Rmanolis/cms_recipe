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
import net.liftweb.http.js.JsCmds.ReplaceOptions

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