package code.snippet

import scala.xml.{ NodeSeq, Text }
import net.liftweb._
import util._
import http._
import common._
import Helpers._

import js.JsCmds._
import code.model._
import code.lib._

class AddCourse{
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
            Course.add(l, name)
        }
        S.redirectTo(Site.crudCourse.url)

      }) &
      "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudCourse.url)

      })
  }
}

class EditCourse{
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      course <- Site.editCourseLoc.currentValue
      language <- course.language.obj
    } yield {
      var name = course.name.is
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
              course.edit(l, name)
          }
          S.redirectTo(Site.crudCourse.url)

        }) &
        "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudCourse.url)

        })).apply(in)
    }
    out
  }
}

class CRUDCourses{
  def render(in: NodeSeq): NodeSeq = {
    import SHtml._

    <table id="listCourses"> {
      <tr>
        <th> Name </th>
        <th> Language </th>
        <th> Edit </th>
        <th> Delete </th>
      </tr> ++
        Course.findAll.map {
          course =>
            <tr> {
              <td> { course.name.is } </td> ++
                <td>{
                  course.getLanguageName
                } </td> ++
                <td> {
                  button(Text("Edit"), () => {
                    S.redirectTo(Site.editCourseLoc.calcHref(course))

                  })
                }</td> ++
                <td> {
                  button(Text("Delete"), () => {
                    course.delete_!
                    S.redirectTo(Site.crudCourse.url)
                  })
                }</td>
            }</tr>
        }
    }</table>

  }
  
}