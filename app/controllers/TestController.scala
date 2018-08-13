package controllers

import java.io.FileNotFoundException

import markov.MarkovParser
import play.api.mvc.{Action, Controller}

class TestController extends Controller{
  def index(text: String) =  Action {

    var sentence = ""
    var word = "\n"

    try {
      val markovParser = new MarkovParser(text + ".txt")
      val markovWords = markovParser.getWords(2)

      val r = scala.util.Random
      var i = r.nextInt(10) + 10
      while (i > 0) {
        word = markovWords.getANextWord(word).get
        if (word != "\n") sentence += word else sentence += "<br>"
        if (word == "\n") i -= 1
      }

    } catch {
      case ioe: FileNotFoundException => sentence = "Could not found example text for " + text
      case e => {
        sentence = "Something unexpected went wrong\n" + e.toString
      }
    }

    Ok(views.html.markov(sentence))
  }
}
