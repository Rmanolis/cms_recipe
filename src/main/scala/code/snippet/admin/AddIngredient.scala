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

class  AddIngredient {
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