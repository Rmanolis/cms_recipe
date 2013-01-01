package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.RecipeModelHelpers._

class Ingredient extends LongKeyedMapper[Ingredient] {
  def getSingleton = Ingredient

  def primaryKeyField = id
  object id extends MappedLongIndex(this)
  object language extends MappedLongForeignKey(this, Language)
  object name extends MappedString(this, 300){
    override def validations = valMinLen(1, "Name cannot be blank") _ ::
      valMaxLen(300, "Name is too long") _ ::
      super.validations
  }
  object foodType extends MappedLongForeignKey(this, FoodType)

  def edit(language: Language, foodType: FoodType, name: String) = {
    this.language(language).foodType(foodType).name(name)
    saveBox(this)
  }

  def getLanguageName = {
    this.language.obj match {
      case Full(lang) => {
        lang.name.is
      }
      case Empty => {

        ""
      }
      case Failure(m, t, s) => {
        ""
      }
    }
  }

  def getFoodTypeName = {
    this.foodType.obj match {
      case Full(ft) => {
        ft.name.is
      }
      case Empty => {

        ""
      }
      case Failure(m, t, s) => {
        ""
      }
    }
  }
  
  def inWhichRecipe = {
    RecipeIngredient.findAll(By(RecipeIngredient.ingredient,this.id.get)).map{
      ri => 
        ri.recipe.obj
    }
  }
  
  def delete{
    RecipeIngredient.findAll(By(RecipeIngredient.ingredient,this)).map(_.delete_!)
    delete_!
  }
}

object Ingredient extends Ingredient with LongKeyedMetaMapper[Ingredient] {
  def add(language: Language, foodType: FoodType, name: String) = {
    val rf = Ingredient.create
    rf.language(language)
    rf.name(name)
    rf.foodType(foodType)
    saveBox(rf)
  }
  def findByLanguage(language: Language) = {
    Ingredient.findAll(By(Ingredient.language, language))
  }

  def findByName(name: String) = {
    Ingredient.find(By(Ingredient.name, name))

  }

  def findByFoodType(ft: FoodType) = {
    Ingredient.findAll(By(Ingredient.foodType, ft))
  }
}



