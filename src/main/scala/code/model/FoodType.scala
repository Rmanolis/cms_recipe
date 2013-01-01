package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.RecipeModelHelpers._

class FoodType extends LongKeyedMapper[FoodType] {
  def getSingleton = FoodType
  def primaryKeyField = id
  object id extends MappedLongIndex(this)
  object language extends MappedLongForeignKey(this, Language)
  object name extends MappedString(this, 140)

  def edit(language: Language, name: String) = {
    this.language(language).name(name).save()
  }

  def getLanguageName = {
    this.language.obj match {
      case Full(lang) => {
        lang.name.is
      }
      case Empty => {

        ""
      }
      case Failure(m, n, s) => {
        ""
      }
    }
  }
  def deleteDependents{
    Ingredient.findAll(By(Ingredient.foodType,this)).map{
      i => i.delete
    }
  }
  def cleanDependents{
    Ingredient.findAll(By(Ingredient.foodType,this)).map{
      i => i.foodType(Empty).save
    }
  }
}

object FoodType extends FoodType with LongKeyedMetaMapper[FoodType] {
  def findByName(name: String) = {
    FoodType.findAll(By(FoodType.name, name))
  }
  def add(language: Language, name: String) = {
    val rf = FoodType.create
    rf.language(language)
    rf.name(name)
    rf.save()
    rf
  }

  def findByLanguage(language: Language) = {
    FoodType.findAll(By(FoodType.language, language))
  }
}