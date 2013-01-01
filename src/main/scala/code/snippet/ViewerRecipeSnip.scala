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
import net.liftmodules.widgets.autocomplete.AutoComplete

class ListOfLatestRecipes {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      lan <- LanguageHelper.getLanguage
    } yield {
      out = ("#listOfLatestRecipes" #> {
        Recipe.findByLanguage(lan).take(5).map {
          re =>
            "li" #> {
              "#name" #> re.name.is &
                "#ingredients" #> re.getIngredients.map(_.name.is).foldLeft("")((s1, s2) => s1 + " " + s2)
            }
        }
      }).apply(in)
    }
    out
  }

}

object ingrResults extends SessionVar[List[Ingredient]](List())

class SearchByIngredient {

  def buildNames(list: List[String], current: String, limit: Int): Seq[String] = {
    val v = current.split(" ").toList.map(_.trim)
    val start = v.dropRight(1).foldLeft[String]("")((s1: String, s2: String) => s1 + " " + s2)
    list.filter {
      s =>
        s.toLowerCase.startsWith(v.last.toLowerCase)

    }.map(start + " " + _)
  }

  def search(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      language <- LanguageHelper.getLanguage
    } yield {

      language
      val list = Ingredient.findByLanguage(language).map {
        i => i.name.is
      }

      out = (
        "#search" #> AutoComplete("", (current, limit) =>
          {
            buildNames(list, current, limit)

          },
          value => {
            val ingrs = value.split(" ").map {
              v =>
                v.trim
                val ingr = v.trim.head.toUpper + v.trim.tail
                Ingredient.findByName(ingr)
            }
            for {
              in <- ingrs
              i <- in
            } yield {
              ingrResults.set(ingrResults.get ++ List(i))
            }
            S.redirectTo(Site.searchByIngredients.url)
          })).apply(in)

    }

    out
  }

  def check(in: NodeSeq): NodeSeq = {
    import scala.collection.mutable.HashMap
    var out = NodeSeq.Empty
    for {
      language <- LanguageHelper.getLanguage
    } yield {
      var listOfChecks: HashMap[Ingredient, Boolean] = Ingredient.findByLanguage(language).foldLeft(HashMap[Ingredient, Boolean]()) {
        (m, i) =>
          m += (i -> false)
      }

      out = ("#listOfCheckBoxByIngredients" #> {
        listOfChecks.map {
          c =>
            "li" #> {

              "#ingredient" #> c._1.name.is &
                "#checkBox" #> SHtml.checkbox(c._2, listOfChecks(c._1) = _)

            }
        }
      } &
        "#submit" #> SHtml.button(Text("Submit"), () => {
          ingrResults.set(listOfChecks.filter(v => v._2 == true).map(_._1).toList)
          S.redirectTo(Site.searchByIngredients.url)
        })).apply(in)

    }
    out
  }

}

class ShowRecipes {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty

    var listOfRecipes: List[Recipe] = List()
    println("RWWWWWWWWWWWW " + ingrResults.mkString)
    for {
      i <- ingrResults.get
    } yield {
      println(i.toString + " " + i.inWhichRecipe.mkString)
      for {

        rbox <- i.inWhichRecipe
        r <- rbox
      } yield {
        listOfRecipes +:= r
      }
    }
    out = (
      "#listOfRecipes" #> listOfRecipes.distinct.map {
        re =>
          "li" #> {
            "#name" #> re.name.is &
              "#ingredients" #> re.getIngredients.map(_.name.is).foldLeft("")((s1, s2) => s1 + " " + s2)
          }
      }).apply(in)
    ingrResults.set(List())
    out

  }
}

class ShowRecipe {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      recipe <- Site.showRecipeLoc.currentValue
    } yield {
      out = (
        "#title" #> recipe.name.is &
        "#course" #> recipe.getCourseName &
        "#listOfIngredients" #> {
          RecipeIngredient.findByRecipe(recipe).map {
            ri =>
              "li" #> {
                "#name" #> ri.getIngredientName &
                  "#quantity" #> ri.quantity.is &
                  "#typeOfMeasure" #> ri.getTypeOfMeasureName
              }
          }
        } &
        "#instructions" #> recipe.instructions.is &
        "#needMachines" #> recipe.needMachines.is &
        "#needCooking" #> recipe.needCooking.is).apply(in)
    }
    out
  }
}

object choosenRecipes extends SessionVar[List[Recipe]](List())

class ChooseRecipes {

  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    choosenRecipes.set(List())
    for {
      language <- LanguageHelper.getLanguage
    } yield {
      val ls = Recipe.findByLanguage(language)
      out = (
        "#listOfLatestRecipes" #> {
          ls.map {
            re =>
              "li" #> {
                "#name *" #> re.name.is &
                "#name [href]" #> Site.showRecipeLoc.calcHref(re) &
                  "#ingredients" #> re.getIngredients.map(_.name.is).foldLeft("")((s1, s2) => s1 + " " + s2) &
                  "#check" #> SHtml.ajaxCheckbox(false , b => {
                    if(b){
                     choosenRecipes.set(choosenRecipes.get ++ List(re))
                    }
                    Noop
                  })
              }
          }
        }&
      "#submit" #> SHtml.button(Text("Submit"),()=>{
        S.redirectTo(Site.neededIngredients.url)
      }) &
      "#cancel" #> SHtml.button(Text("Cancel"),()=>{
        S.redirectTo(Site.home.url)
      })
      ).apply(in)
    }

    out
  }

}

class NeededIngredients{
  def render={
    var ingredients:List[Ingredient] = List()
    choosenRecipes.get.map{
      cr => 
       RecipeIngredient.findByRecipe(cr).map{
         ri => 
           ri.ingredient.obj.map{
             i=>
             ingredients = ingredients ++ List(i)
           }
       }

    }
    "#listOfIngredients" #> {
      ingredients.map{
        i =>
          "li" #>{
            "#name" #> i.name.is
          }
      }
    }
  }
}
