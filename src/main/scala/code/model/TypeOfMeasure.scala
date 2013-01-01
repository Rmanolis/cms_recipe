package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.RecipeModelHelpers._

class TypeOfMeasure extends LongKeyedMapper[TypeOfMeasure] {
  def getSingleton = TypeOfMeasure

  def primaryKeyField = id
  object id extends MappedLongIndex(this)
  object name extends MappedString(this, 140){
    override def validations = valMinLen(1, "Name cannot be blank") _ ::
      valMaxLen(300, "Name is too long") _ ::
      super.validations
  }
  object language extends MappedLongForeignKey(this, Language)
  
  def edit(language:Language,name:String)={
    this.name(name).language(language)
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
      case Failure(m, n, s) => {
        ""
      }
    }
  }
   
   def deleteDependents{
     RecipeIngredient.findAll(By(RecipeIngredient.measure,this)).map(_.delete_!)
   }
   def cleanDependents{
     RecipeIngredient.findAll(By(RecipeIngredient.measure,this)).map(_.measure(Empty).save)
   }
   
   def delete{
     deleteDependents //I choose to delete dependents because RecipeIngredients do not have edit.
     delete_!
   }
}

object TypeOfMeasure extends TypeOfMeasure with LongKeyedMetaMapper[TypeOfMeasure]{
  def add(language:Language,name:String)={
    val tm = TypeOfMeasure.create
    tm.name(name)
    tm.language(language)
    saveBox(tm)
  }
  def findByLanguage(language:Language) = {
    TypeOfMeasure.findAll(By(TypeOfMeasure.language,language))
  }
  
  def findByName(name:String) = {
    TypeOfMeasure.find(By(TypeOfMeasure.name,name))
  }
}