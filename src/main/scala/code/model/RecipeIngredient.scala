package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class RecipeIngredient extends LongKeyedMapper[RecipeIngredient] {
  def getSingleton = RecipeIngredient

  def primaryKeyField = id
  object id extends MappedLongIndex(this)
  object recipe extends MappedLongForeignKey(this, Recipe)
  object ingredient extends MappedLongForeignKey(this, Ingredient)
  object quantity extends MappedInt(this)
  object measure extends MappedLongForeignKey(this, TypeOfMeasure)
  
  def edit(ingredient:Ingredient,number:Int,measure:TypeOfMeasure)={
    this.ingredient(ingredient).quantity(number).measure(measure).save
  }
  
  def getIngredientName = {
    this.ingredient.obj match {
      case Full(ingre) => {
        ingre.name.is
      }
      case Empty => {
        ""
      }
      case Failure(m, t, s) => {
        ""
      }
    }
  }
  
  def getTypeOfMeasureName = {
    this.measure.obj match {
      case Full(meau) => {
        meau.name.is
      }
      case Empty => {
        ""
      }
      case Failure(m, t, s) => {
        ""
      }
    }
  }

}

object RecipeIngredient extends RecipeIngredient with LongKeyedMetaMapper[RecipeIngredient]{
  def add(recipe:Recipe,ingredient:Ingredient,measure:TypeOfMeasure , number:Int)={
    val i = RecipeIngredient.create
    i.recipe(recipe)
    i.ingredient(ingredient)
    i.quantity(number)
    i.measure(measure)
    i.save
    i
  }
  def findByRecipe(recipe : Recipe)={
    RecipeIngredient.findAll(By(RecipeIngredient.recipe,recipe.id.get))
  }
}


