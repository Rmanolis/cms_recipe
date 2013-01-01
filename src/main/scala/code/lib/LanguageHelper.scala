package code.lib
import net.liftweb._
import common._
import http._
import code.model.Language


object languageSession extends SessionVar[Box[Language]](Empty)

object LanguageHelper {
  def getLanguage : Box[Language] = {
    S.param("language").map {
      lang =>
        val lan = Language.findByName(lang)
        languageSession.set(lan )
       
    }.getOrElse {
      var langOpt: Box[Language] = None
      if (languageSession.get.isEmpty) {
        langOpt = Language.findDefault
      } else {
        langOpt = languageSession.get
      }
      langOpt
    }

  }
}