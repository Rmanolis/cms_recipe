package code.snippet
import scala.xml.{NodeSeq, Text}
import net.liftweb._
import util._
import http._
import common._
import Helpers._

import js.JsCmds._
import code.model._
import code.lib._

class AddLocalization {
  
  def render = {
    var label = ""
    var value = ""
    var languages = Language.findAll().map{
      lan => 
        (lan.name.is,lan.name.is)
    } 
    var language = ""
    
    "#label" #> SHtml.text("", label = _) &
      "#text" #> SHtml.textarea("", value = _) &
      "#languages" #> SHtml.select(languages, Empty, language = _, "id" -> "languages") &
      "#addButton" #> SHtml.button(Text("Submit"), () => {
        Language.findByName(language).map {
          l =>
            Localization.add(label, value, l)
        }
        S.redirectTo(Site.crudLocalization.url)

      }) &
      "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudLocalization.url)

      })
  }
  
}

class EditLocalization {
  
  def render(in:NodeSeq):NodeSeq = {
    var out = NodeSeq.Empty
    for {
      ll <- Site.editLocalizationLoc.currentValue
      language <- ll.language.obj
    } yield {
      val languages = Language.findAll.map(l => (l.id.is.toString, l.name.is))
      var label = ll.label.is
      var text = ll.text.is
      var lan =  language.name.is
      out = ("#label" #> SHtml.text(label, label = _) &
        "#text" #> SHtml.textarea(text, text = _) &
        "#languages" #> SHtml.select(languages, Full(lan), lan = _, "id" -> "languages") &
        "#addButton" #> SHtml.button(Text("Submit"), () => {
          Language.findByName(lan).map {
            l =>
              ll.edit(label, text, l)
          }
          S.redirectTo(Site.crudLocalization.url)

        }) &
        "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudLocalization.url)

        })).apply(in)
    }
    out
  }
  
}

class CRUDLocalizations{
  def render = {
    <table id="listOfLocalesByLabel"> {
      <tr>
        <th> Label </th>
        <th> Text </th>
        <th> Language </th>
        <th> Edit </th>
        <th> Delete </th>
      </tr> ++
        Localization.findAll.sortBy(_.label.is).map {
          ll =>
            <tr> {
              <td> {ll.label.is }</td> ++
                <td> {ll.text.is }</td> ++
                <td> {ll.getLanguageName } </td> ++
                <td>{ SHtml.button(Text("Edit"), () => {
                  S.redirectTo(Site.editLocalizationLoc.calcHref(ll))

                }) }</td> ++
                <td> {SHtml.button(Text("Delete"), () => {
                  ll.delete_!
                  S.redirectTo(Site.crudLocalization.url)

                }) }</td>
            }</tr>
        }
    } </table>
  }
  
}