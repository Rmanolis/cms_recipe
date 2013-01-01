package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.RecipeModelHelpers._

class Localization extends LongKeyedMapper[Localization] with IdPK {
  def getSingleton = Localization
  object label extends MappedString(this, 140){
    override def validations = valMinLen(1, "Label cannot be blank") _ ::
      valMaxLen(140, "Label is too long") _ ::
      super.validations
  }
  object text extends MappedText(this)
  object language extends MappedLongForeignKey(this, Language)
  def edit(label: String, value: String, language: Language) = {
    this.label(label.trim).text(value).language(language)
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
}

object Localization extends Localization with LongKeyedMetaMapper[Localization] {
  def add(label: String, value: String, language: Language) = {
    val l = Localization.create
    l.label(label.trim).text(value).language(language)
    saveBox(l)
  }

  def findByLabel(title: String) = {
    Localization.findAll(By(Localization.label, title))
  }

  def findByLabelAndLanguage(label: String, language: Language) = {
    Localization.findAll(BySql("label = ? AND language_c = ?",
      IHaveValidatedThisSQL("mragias", "2012-11-02"),
      label, language.id.is)).headOption

  }

}