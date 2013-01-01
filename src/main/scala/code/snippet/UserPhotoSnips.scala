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

import net.liftweb.mapper._

private object theUpload extends RequestVar[Box[FileParamHolder]](Empty)

class AddPhotoToRecipe {

  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      recipe <- Site.addPhotoLoc.currentValue
    } yield {
      var isMain = false
      var text = ""
      out=("#upload" #> SHtml.fileUpload { fp =>
        theUpload(Full(fp))
      } &
        "#isMain" #> SHtml.checkbox(isMain, isMain = _) &
        "#text" #> SHtml.textarea(text, text = _) &
        "#submit" #> SHtml.button(Text("Submit"), () => {

          for {
            up <- theUpload.is
            photo <- Photo.add(up.mimeType)
            pu <- PhotoRecipe.add(recipe, photo, isMain, text)
          } yield {

            ImageHelpers.save(up.fileStream, Props.get("recipe.photo.source").openOr("src/main/webapp/photos/") + photo.name.is)
            ImageHelpers.saveBI(ImageHelpers.resize(up.fileStream, 200, 200), Props.get("recipe.photo.source").openOr("src/main/webapp/photos/") + photo.minName.is)
            S.redirectTo(Site.addPhotoLoc.calcHref(recipe))
          }

        }) &
        "#cancel" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudRecipe.fullUrl)
        })).apply(in)
    }

    out
  }

}

class PhotoTable extends PaginatorSnippet[PhotoRecipe] {
  override def count = PhotoRecipe.count
  override def itemsPerPage = 10

  override def page = {
    var list: List[QueryParam[PhotoRecipe]] = List()
    Site.addPhotoLoc.currentValue.map {
      rec =>
        list +:= By(PhotoRecipe.recipe, rec)
    }
    list +:= OrderBy(PhotoRecipe.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    PhotoRecipe.findAll(list: _*)
  }
  
  def renderPage(in: NodeSeq): NodeSeq = {
    <table title="Photos">
      <thead>
        <tr>
          <th> Image </th>
          <th> Main </th>
          <th> Delete </th>
        </tr>
      </thead>{
        page.map {
          photoRecipe =>
            <tr>  {
                <td> { photoRecipe.imgMiniPhoto(50,50)}</td> ++
                <td> { if(photoRecipe.isMain.is){
                  SHtml.button(Text("Remove as main photo"),()=>{
                    photoRecipe.isMain(false).save
                    S.redirectTo(S.uri)
                  })
                }else{
                  SHtml.button(Text("Put as main photo"),()=>{
                    photoRecipe.makeItTheMain
                    S.redirectTo(S.uri)
                  })
                }}</td> ++
                <td> {
                  SHtml.button(Text("Delete"), () => {
                    photoRecipe.delete
                    S.redirectTo(S.uri)
                  })
                } </td>
            }</tr>
        }
      }
    </table>
  }
}

