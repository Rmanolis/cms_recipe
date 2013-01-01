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

class ListOfLanguages {
  def render = {
    "#listOfLanguages" #> {
      Language.findAll.map {
        lan =>
          LanguageHelper.getLanguage.map {
            currentLanguage =>
              if (lan != currentLanguage) {
                "li" #> SHtml.ajaxButton(Text(lan.name.is), () => {
                  languageSession.set(Full(lan))
                  RedirectTo("/")

                })
              } else {
                "li" #> NodeSeq.Empty
              }
          }.getOrElse {
            "li" #> SHtml.ajaxButton(Text(lan.name.is), () => {
              languageSession.set(Full(lan))
              RedirectTo("/")

            })
          }

      }
    }
  }
}