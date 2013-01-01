package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S
import code.lib._
import RecipeModelHelpers._
class Recipe extends LongKeyedMapper[Recipe] {
  def getSingleton = Recipe

  def primaryKeyField = id
  object id extends MappedLongIndex(this)
  object name extends MappedString(this, 300) {
    override def validations = valMinLen(1, "Name cannot be blank") _ ::
      valMaxLen(300, "Name is too long") _ ::
      super.validations
  }
  object author extends MappedLongForeignKey(this, User)
  object language extends MappedLongForeignKey(this, Language)
  object course extends MappedLongForeignKey(this, Course)
  object needCooking extends MappedBoolean(this)
  object needMachines extends MappedBoolean(this)
  object instructions extends MappedText(this)

  def edit(user: User, language: Language, course: Course, name: String, needC: Boolean, needM: Boolean, inst: String) = {
    this.name(name).author(user).language(language).course(course).needCooking(needC).needMachines(needM).instructions(inst)
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

  def getCourseName = {
    this.course.obj match {
      case Full(cou) => {
        cou.name.is
      }
      case Empty => {

        ""
      }
      case Failure(m, t, s) => {
        ""
      }
    }
  }

  def getAuthorName = {
    this.author.obj match {
      case Full(auth) => {
        auth.niceName
      }
      case Empty => {

        ""
      }
      case Failure(m, t, s) => {
        ""
      }
    }
  }

  def delete = {
    RecipeIngredient.findAll(By(RecipeIngredient.ingredient, this.id.get)).map {
      ri =>
        ri.delete_!
    }
    this.delete_!
  }
  def getIngredients = {
    RecipeIngredient.findAll(By(RecipeIngredient.recipe, this.id.get)).map {
      ri =>
        ri.ingredient.obj.get
    }
  }

}

object Recipe extends Recipe with LongKeyedMetaMapper[Recipe] {

  def add(user: User, language: Language, course: Course, name: String, needC: Boolean, needM: Boolean, inst: String): Box[Recipe] = {
    val r = Recipe.create
    r.author(user)
      .name(name)
      .language(language)
      .course(course)
      .needCooking(needC)
      .needMachines(needM)
      .instructions(inst)
    saveBox(r)

  }
  def justCreateOne = {
   val r = Recipe.create
   saveBox(r)
  }

  def findByLanguage(lan: Language) = {
    Recipe.findAll(By(Recipe.language, lan))
  }
}



