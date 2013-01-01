package code.snippet
import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import util._
import http._
import common._
import Helpers._

import js._
import js.JsCmds._
import js.JE._
import code.model._
import code.lib._

class AddRecipe extends StatefulSnippet {
  private val recipe = Recipe.justCreateOne
  private val langs = Language.findAll()
  private var languages = langs.map {
    lan =>
      (lan.name.is, lan.name.is)
  }
  private var language = languages.head._1

  private var foodTypes = List(("", "")) ++ FoodType.findByLanguage(langs.head).map {
    ft => (ft.name.is, ft.name.is)
  }
  private var ingredients = List(("", "")) ++ Ingredient.findByLanguage(langs.head).map {
    i => (i.name.is, i.name.is)
  }
  private var measureOfTypes = List(("", "")) ++ TypeOfMeasure.findByLanguage(langs.head).map {
    tom => (tom.name.is, tom.name.is)
  }

  def dispatch = {
    case "addRecipe" => addRecipe
    case "addIngredient" => addIngredient
  }
  def addRecipe(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    var name = ""
    var instructions = ""
    var needCooking = false
    var needMachines = false

    for {
      fl <- langs.headOption
    } yield {
      var courses = List(("", "")) ++ Course.findByLanguage(fl).map {
        cou =>
          (cou.name.is, cou.name.is)
      }
      var course = ""
      out = ("#title" #> SHtml.text(name, name = _) &
        "#instructions" #> SHtml.textarea(instructions, instructions = _) &
        "#needCooking" #> SHtml.checkbox(needCooking, needCooking = _) &
        "#needMachines" #> SHtml.checkbox(needMachines, needMachines = _) &
        "#languages" #> SHtml.select(languages, Empty, s => {
          language = s
          val lang = Language.findByName(language).head
          courses = Course.findByLanguage(lang).map {
            cou =>
              (cou.name.is, cou.name.is)
          }
          foodTypes = FoodType.findByLanguage(lang).map {
            ft => (ft.name.is, ft.name.is)
          }
          ingredients = Ingredient.findByLanguage(lang).map {
            i => (i.name.is, i.name.is)
          }
          measureOfTypes = TypeOfMeasure.findByLanguage(lang).map {
            tom => (tom.name.is, tom.name.is)
          }

          ReplaceOptions("courses", List(("", "")) ++ courses, Full("")) &
            ReplaceOptions("foodTypes", List(("", "")) ++ foodTypes, Full("")) &
            ReplaceOptions("measure", List(("", "")) ++ measureOfTypes, Full("")) &
            ReplaceOptions("ingredients", List(("", "")) ++ ingredients, Full(""))

        }, "id" -> "languages") &
        "#courses" #> SHtml.ajaxUntrustedSelect(courses, Empty, course = _, "id" -> "courses") &
        "#addButton" #> SHtml.button(Text("Submit"), () => {
          for {
            user <- User.currentUser
            lang <- Language.findByName(language)
            cou <- Course.findByName(course)
            r <- recipe
          } yield {
            r.edit(user, lang, cou, name, needCooking, needMachines, instructions)

          }
          S.redirectTo(Site.crudRecipe.url)

        }) &
        "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
          recipe.map(_.delete)
          S.redirectTo(Site.crudRecipe.url)

        })).apply(in)
    }
    out
  }
  object IngredientsJson extends JsonHandler {
    def apply(in: Any): JsCmd = {
      def rec(): JsCmd = {
        recipe.map { r =>
          SetHtml("json_result", in match {
            case JsonCmd("show", _, "list", _) => {
              <table id="json_result"> {
                <tr>
                  <th> Ingredient </th>
                  <th> Quantity </th>
                  <th> Type of measure</th>
                  <th> Delete </th>
                </tr> ++
                  RecipeIngredient.findByRecipe(r).map {
                    ri =>
                      <tr>{
                        <td>{ ri.getIngredientName } </td> ++
                          <td> { ri.quantity.is } </td> ++
                          <td> { ri.getTypeOfMeasureName } </td> ++
                          <td>{
                            SHtml.ajaxButton(Text("delete"), () => {
                              ri.delete_!
                              rec
                            })
                          }</td>
                      }</tr>
                  }

              }</table>

            }
            case x => <b>Problem... didn't handle JSON message { x }</b>
          })
        }.getOrElse(Noop)
      }
      rec
    }
  }

  def addIngredient(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty

    var foodType = ""
    var ingredient = ""
    var quantity = ""
    var measure = ""

    out = (
      "#json_script" #> Script(IngredientsJson.jsCmd) &
      "#foodTypes" #> SHtml.ajaxUntrustedSelect(foodTypes, Empty, s => {
        foodType = s
        for {
          ft <- FoodType.findByName(foodType)
        } yield {
          ingredients = Ingredient.findByFoodType(ft).map {
            i => (i.name.is, i.name.is)
          }

        }
        ReplaceOptions("ingredients", List(("", "")) ++ ingredients, Full(""))
      }, "id" -> "foodTypes") &
      "#measure" #> SHtml.ajaxUntrustedSelect(List(("", "")) ++ measureOfTypes, Full(""), measure = _, "id" -> "measure") &
      "#ingredients" #> SHtml.ajaxUntrustedSelect(List(("", "")) ++ ingredients, Full(""), s => {
        ingredient = s
        println("REEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE " + s)
        Noop
      }, "id" -> "ingredients") &
      "#quantity" #> SHtml.ajaxText(quantity, quantity = _) &
      "#add" #> SHtml.ajaxButton(Text("Add"), () => {

        for {
          ing <- Ingredient.findByName(ingredient)
          num <- asInt(quantity)
          tom <- TypeOfMeasure.findByName(measure)
          r <- recipe
        } yield {
          RecipeIngredient.add(r, ing, tom, num)

        }

        JsRaw(IngredientsJson.call("show", "list")).cmd
      })).apply(in)

    out
  }

}

class CRUDRecipe {
  def render = {
    <table id="listRecipes"> {
      <tr>
        <th> Title </th>
        <th> Author </th>
        <th> Language </th>
    	<th>Add Photo </th>
        <th> Edit </th>
        <th> Delete </th>
      </tr> ++
        Recipe.findAll.map {
          ri =>
            <tr> {
              <td>{ ri.name.is } </td> ++
                <td> { ri.getAuthorName } </td> ++
                <td> { ri.getLanguageName } </td> ++
                <td>{
                  SHtml.button(Text("Add Photo"), () => {
                    S.redirectTo(Site.addPhotoLoc.calcHref(ri))
                  })
                }</td> ++
                <td>{
                  SHtml.button(Text("edit"), () => {
                    S.redirectTo(Site.editRecipeLoc.calcHref(ri))
                  })
                }</td> ++
                <td>{
                  SHtml.button(Text("delete"), () => {
                    ri.delete
                    S.redirectTo(Site.crudRecipe.url)
                  })
                }</td>
            }</tr>
        }
    }</table>
  }
}

class EditRecipe extends StatefulSnippet {
  private val recipeBox = Site.editRecipeLoc.currentValue
  private val langs = Language.findAll
  private val languages = langs.map {
    lan =>
      (lan.name.is, lan.name.is)
  }

  private var language = ""

  private var courses: List[(String, String)] = List()
  private var foodTypes: List[(String, String)] = List()
  private var ingredients: List[(String, String)] = List()
  private var measures: List[(String, String)] = List()

  def dispatch = {
    case "editRecipe" => editRecipe
    case "addIngredient" => addIngredient
    case "tableOfIngredients" => tableOfIngredients
  }
  def editRecipe(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      recipe <- recipeBox
      langObj <- recipe.language.obj
    } yield {
      var language = recipe.getLanguageName
      var course = recipe.getCourseName
      var title = recipe.name.is
      var instructions = recipe.instructions.is
      var needCooking = recipe.needCooking.is
      var needMachines = recipe.needMachines.is

      foodTypes = FoodType.findByLanguage(langObj).map {
        ft => (ft.name.is, ft.name.is)
      }
      ingredients = Ingredient.findByLanguage(langObj).map {
        i => (i.name.is, i.name.is)
      }
      measures = TypeOfMeasure.findByLanguage(langObj).map {
        tom => (tom.name.is, tom.name.is)
      }

      courses = Course.findByLanguage(langObj).map {
        cou =>
          (cou.name.is, cou.name.is)
      }
      out = (
        "#title" #> SHtml.ajaxText(title, title = _) &
        "#instructions" #> SHtml.ajaxTextarea(instructions, instructions = _) &
        "#needCooking" #> SHtml.ajaxCheckbox(needCooking, needCooking = _) &
        "#needMachines" #> SHtml.ajaxCheckbox(needMachines, needMachines = _) &
        "#languages" #> SHtml.ajaxSelect(languages, Empty, s => {
          language = s
          val lang = Language.findByName(language).head
          courses = Course.findByLanguage(lang).map {
            cou =>
              (cou.name.is, cou.name.is)
          }
          foodTypes = FoodType.findByLanguage(lang).map {
            ft => (ft.name.is, ft.name.is)
          }
          ingredients = Ingredient.findByLanguage(lang).map {
            i => (i.name.is, i.name.is)
          }
          measures = TypeOfMeasure.findByLanguage(lang).map {
            tom => (tom.name.is, tom.name.is)
          }

          ReplaceOptions("courses", List(("", "")) ++ courses, Full("")) &
            ReplaceOptions("foodTypes", List(("", "")) ++ foodTypes, Full("")) &
            ReplaceOptions("measure", List(("", "")) ++ measures, Full("")) &
            ReplaceOptions("ingredients", List(("", "")) ++ ingredients, Full(""))

        }, "id" -> "languages") &
        "#courses" #> SHtml.ajaxUntrustedSelect(courses, Empty, course = _, "id" -> "courses") &
        "#addButton" #> SHtml.ajaxButton(Text("Submit"), () => {
          for {
            user <- User.currentUser
            lang <- Language.findByName(language)
            cou <- Course.findByName(course)
          } yield {
            recipe.edit(user, lang, cou, title, needCooking, needMachines, instructions)
          }
          RedirectTo(Site.crudRecipe.url)
        }) &
        "#cancelButton" #> SHtml.ajaxButton(Text("Cancel"), () => {
          RedirectTo(Site.crudRecipe.url)

        })).apply(in)

    }

    out
  }

  object IngredientsJson extends JsonHandler {
    def apply(in: Any): JsCmd = {

      SetHtml("json_result", in match {
        case JsonCmd("show", _, "list", _) => {
          recipeBox.map {
            recipe =>

              <table id="json_result"> {
                <tr>
                  <th> Ingredient </th>
                  <th> Quantity </th>
                  <th> Type of measure</th>
                  <th> Delete </th>
                </tr> ++
                  RecipeIngredient.findByRecipe(recipe).map {
                    ri =>
                      <tr>{
                        <td>{ ri.getIngredientName } </td> ++
                          <td> { ri.quantity.is } </td> ++
                          <td> { ri.getTypeOfMeasureName } </td> ++
                          <td>{
                            SHtml.ajaxButton(Text("delete"), () => {
                              ri.delete_!
                              JsRaw(IngredientsJson.call("show", "list")).cmd

                            })
                          }</td>
                      }</tr>
                  }

              }</table>
          }.getOrElse(<table id="json_result"></table>)

        }
        case x => <b>Problem... didn't handle JSON message { x }</b>
      })

    }
  }

  def tableOfIngredients(in: NodeSeq): NodeSeq = {
    recipeBox.map {
      recipe =>

        <table id="json_result"> {
          <tr>
            <th> Ingredient </th>
            <th> Quantity </th>
            <th> Type of measure</th>
            <th> Delete </th>
          </tr> ++
            RecipeIngredient.findByRecipe(recipe).map {
              ri =>
                <tr>{
                  <td>{ ri.getIngredientName } </td> ++
                    <td> { ri.quantity.is } </td> ++
                    <td> { ri.getTypeOfMeasureName } </td> ++
                    <td>{
                      SHtml.ajaxButton(Text("delete"), () => {
                        ri.delete_!
                        JsRaw(IngredientsJson.call("show", "list")).cmd
                      })
                    }</td>
                }</tr>
            }

        }</table>
    }.getOrElse(<table id="json_result"></table>)

  }

  def addIngredient(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty

    var foodType = ""
    var ingredient = ""
    var quantity = ""
    var measure = ""
    for {
      recipe <- recipeBox
    } yield {
      out = (
        "#json_script" #> Script(IngredientsJson.jsCmd) &
        "#foodTypes" #> SHtml.ajaxUntrustedSelect(foodTypes, Empty, s => {
          foodType = s
          for {
            ft <- FoodType.findByName(foodType)
          } yield {
            ingredients = Ingredient.findByFoodType(ft).map {
              i => (i.name.is, i.name.is)
            }

          }
          ReplaceOptions("ingredients", List(("", "")) ++ ingredients, Full(""))
        }, "id" -> "foodTypes") &
        "#measure" #> SHtml.ajaxUntrustedSelect(List(("", "")) ++ measures, Full(""), measure = _, "id" -> "measure") &
        "#ingredients" #> SHtml.ajaxUntrustedSelect(List(("", "")) ++ ingredients, Full(""), s => {
          ingredient = s
          Noop
        }, "id" -> "ingredients") &
        "#quantity" #> SHtml.ajaxText(quantity, quantity = _) &
        "#add" #> SHtml.ajaxButton(Text("Add"), () => {

          for {
            ing <- Ingredient.findByName(ingredient)
            num <- asInt(quantity)
            tom <- TypeOfMeasure.findByName(measure)
          } yield {
            RecipeIngredient.add(recipe, ing, tom, num)

          }

          JsRaw(IngredientsJson.call("show", "list")).cmd
        })).apply(in)
    }

    out
  }

}