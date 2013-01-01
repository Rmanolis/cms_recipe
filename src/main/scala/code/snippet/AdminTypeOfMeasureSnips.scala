package code.snippet

import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import util._
import http._
import common._
import Helpers._
import mapper._

import js.JsCmds._
import code.model._
import code.lib._


class AddTypeOfMeasure{
  def render = {
    var name = ""
    var languages = Language.findAll().map {
      lan =>
        (lan.name.is, lan.name.is)
    }
    var language = ""
    "#name" #> SHtml.text(name, name = _) &
      "#languages" #> SHtml.select(languages, Empty, language = _, "id" -> "languages") &
      "#addButton" #> SHtml.button(Text("Submit"), () => {
        Language.findByName(language).map {
          l =>
            TypeOfMeasure.add(l, name)
        }
        S.redirectTo(Site.crudTypeOfMeasure.url)

      }) &
      "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudTypeOfMeasure.url)

      })
  }
}

class EditTypeOfMeasure{
   def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      typeOfMeasure <- Site.editCourseLoc.currentValue
      language <- typeOfMeasure.language.obj
    } yield {
      var name = typeOfMeasure.name.is
      var languages = Language.findAll().map {
        lan =>
          (lan.name.is, lan.name.is)
      }
      var lang = language.name.is

      out = (
        "#name" #> SHtml.text(name, name = _) &
        "#languages" #> SHtml.select(languages, Full(lang), lang = _, "id" -> "languages") &
        "#addButton" #> SHtml.button(Text("Submit"), () => {
          Language.findByName(lang).map {
            l =>
              typeOfMeasure.edit(l, name)
          }
          S.redirectTo(Site.crudTypeOfMeasure.url)

        }) &
        "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudTypeOfMeasure.url)

        })).apply(in)
    }
    out
  }
}

class CRUDTypeOfMeasures extends PaginatorSnippet[TypeOfMeasure] {
  override def count = TypeOfMeasure.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[QueryParam[TypeOfMeasure]] = List()
    list +:= OrderBy(TypeOfMeasure.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    TypeOfMeasure.findAll(list: _*)
  }
  def renderPage(in: NodeSeq): NodeSeq = {
    import SHtml._

    <table id="listTypeOfMeasures"> {
      <tr>
        <th> Name </th>
        <th> Language </th>
        <th> Edit </th>
        <th> Delete </th>
      </tr> ++
        page.map {
          tom =>
            <tr> {
              <td> { tom.name.is } </td> ++
                <td>{
                  tom.getLanguageName
                } </td> ++
                <td> {
                  button(Text("Edit"), () => {
                    S.redirectTo(Site.editTypeOfMeasureLoc.calcHref(tom))

                  })
                }</td> ++
                <td> {
                  button(Text("Delete"), () => {
                    tom.delete
                    S.redirectTo(Site.crudTypeOfMeasure.url)
                  })
                }</td>
            }</tr>
        }
    }</table>

  }
}