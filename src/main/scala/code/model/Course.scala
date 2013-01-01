package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class Course extends LongKeyedMapper[Course] {
  def getSingleton = Course

  def primaryKeyField = id
  object id extends MappedLongIndex(this)
  object name extends MappedString(this, 300){
    override def validations = valMinLen(1, "Name cannot be blank") _ ::
      valMaxLen(300, "Name is too long") _ ::
      super.validations
  }
  object language extends MappedLongForeignKey(this, Language)
  
  
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
      case Failure(m, t, s) => {
        ""
      }
    }
  }
  
  def cleanDependents{
    Recipe.findAll(By(Recipe.course,this)).map{
      re => re.course(Empty).save
    }
  }
  
  def delete{
    cleanDependents
    delete_!
  }
  
  def deleteDependents{
    Recipe.findAll(By(Recipe.course,this)).map{
      re => re.delete
    }
  }

}

object Course extends Course with LongKeyedMetaMapper[Course] {
  def add(language: Language, name: String) = {
    val c = Course.create
    c.language(language).name(name).save()
    c
  }
  
  def findByLanguage(language:Language)={
    Course.findAll(By(Course.language,language))
  }
  def findByName(name:String)={
    Course.find(By(Course.name,name))
  }
}