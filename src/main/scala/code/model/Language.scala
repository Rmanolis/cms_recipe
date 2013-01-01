package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.RecipeModelHelpers._

class Language extends LongKeyedMapper[Language] with IdPK {
  def getSingleton = Language
  object name extends MappedString(this, 140) {
    override def validations = valMinLen(1, "Name cannot be blank") _ ::
      valMaxLen(140, "Name is too long") _ ::
      super.validations
  }
  object isDefault extends MappedBoolean(this)
  def edit(name: String, default: Boolean) = {
    if (default) {
      Language.findAll.map {
        lan =>
          lan.isDefault(false).save
      }
    }
    this.name(name).isDefault(default)
    saveBox(this)
  }

  def cleanDependents {
    Course.findByLanguage(this).map(_.language(Empty).save)
    Ingredient.findByLanguage(this).map(_.language(Empty).save)
    Localization.findAll(By(Localization.language, this)).map(_.language(Empty).save)
    Recipe.findByLanguage(this).map(_.language(Empty).save)
    TypeOfMeasure.findAll(By(TypeOfMeasure.language,this)).map(_.language(Empty).save)
  }
  def deleteDependents {
    Course.findByLanguage(this).map(_.delete)
    Ingredient.findByLanguage(this).map(_.delete)
    Localization.findAll(By(Localization.language, this)).map(_.delete_!)
    Recipe.findByLanguage(this).map(_.delete)
    TypeOfMeasure.findAll(By(TypeOfMeasure.language,this)).map(_.delete)
  }
  
}

object Language extends Language with LongKeyedMetaMapper[Language] {
  def add(name: String, default: Boolean) = {
    val l = Language.create
    if (default) {
      Language.findAll.map {
        lan =>
          lan.isDefault(false).save
      }
    }
    l.name(name).isDefault(default)
    saveBox(l)
  }
  def findByName(title: String) = {
    Language.find(By(Language.name, title))
  }

  def findDefault = {
    Language.find(By(Language.isDefault, true))
  }
}
