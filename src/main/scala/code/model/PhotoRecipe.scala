package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib.RecipeModelHelpers._
import scala.xml.NodeSeq

class PhotoRecipe extends LongKeyedMapper[PhotoRecipe] with IdPK {
  def getSingleton = PhotoRecipe
  object recipe extends MappedLongForeignKey(this, Recipe)
  object photo extends MappedLongForeignKey(this, Photo)
  object isMain extends MappedBoolean(this)
  object explanation extends MappedText(this)

  def imgPhoto(height: Int, width: Int) = {
    <img src={ "/photo/" + photo.is } height={ height.toString } width={ width.toString }/>
  }
  def imgMiniPhoto(height: Int, width: Int) = {
    <img src={ "/miniphoto/" + photo.is } height={ height.toString } width={ width.toString }/>

  }

  def delete {
    photo.obj.map(_.delete)
    this.delete_!
  }

  def makeItTheMain {
    this.recipe.obj.map {
      rec =>
        PhotoRecipe.removeMainsFromTheRecipe(rec)
        this.isMain(true).save
    }
  }

}

object PhotoRecipe extends PhotoRecipe with LongKeyedMetaMapper[PhotoRecipe] {
  def add(rec: Recipe, photo: Photo, isMain: Boolean, text: String) = {
    val pu = PhotoRecipe.create
    if (isMain) {
      removeMainsFromTheRecipe(rec)
    }
    pu.recipe(rec).photo(photo).isMain(isMain).explanation(text)
    saveBox(pu)

  }

  def removeMainsFromTheRecipe(rec: Recipe) = {
    PhotoRecipe.findAll(By(PhotoRecipe.recipe, rec)).map(_.isMain(false).save)
  }
}