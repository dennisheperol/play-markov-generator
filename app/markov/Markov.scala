package markov

import java.io.BufferedReader

import scala.collection.mutable
import scala.io.Source
import scala.util.Random

class MarkovWords {
  var _words: mutable.HashMap[String, mutable.HashMap[String, Int]] = new mutable.HashMap

  def addWord(word: String, theNextWord: String): Unit = {
    if (_words.contains(word)) {
      val nextWords = _words(word)
      if(nextWords.contains(theNextWord)) {
        val amountOfWords: Int = nextWords(theNextWord)
        nextWords += ((theNextWord, amountOfWords+1))
      }
      else { // add new nextWord
        nextWords += ((theNextWord, 1))
      }
    }
    else { // add new root word
      val nextWords = new mutable.HashMap[String,Int]()
      nextWords += ((theNextWord, 1))
      _words += ((word, nextWords))
    }
  }

  def getANextWord(word: String): Option[String] = {
    if (!_words.contains(word)) Option.empty
    else {
      val nextWords = _words(word)
      val total: Int = nextWords.values.sum
      var random = Random.nextInt(total)
      var keyList = nextWords.keySet.toList
      var key: String = null
      while (random >= 0) {
        key = keyList.last
        keyList = keyList.init
        random -= nextWords(key)
      }
      Option(key)
    }
  }
}

class MarkovParser(val filename: String) {
  def getWords(amount: Int = 1) : MarkovWords = {
    val markovWords = new MarkovWords

    val inputstream = play.Play.application().classloader().getResourceAsStream(filename)

    for (line <- scala.io.Source.fromInputStream(inputstream).getLines()) { //Source.fromFile("app/texts/"+ filename).getLines) {
      parseLine(line, markovWords, amount)
    }
    markovWords
  }

  def parseLine(line: String, markovWords: MarkovWords, amount: Int = 1): Unit = {
    val array =  line.split(" +")
    for (i <- 0 to array.length - amount * 2) {
      markovWords.addWord(
        getWordsFromLine(array, i, amount),
        getWordsFromLine(array, i + amount, amount))
    }
    if (array.nonEmpty) {markovWords.addWord("\n", getWordsFromLine(array, 0, amount))}
    parseLastWords(array, markovWords, amount)
  }

  def parseLastWords(words: Array[String], markovWords: MarkovWords, amount: Int): Unit = {
    for(
      i <- (words.length + 1 - amount * 2) to (words.length - amount)
      if i >= 0
    ) {
      val first = getWordsFromLine(words, i, amount)
      val last = getWordsFromLine(words, i + amount, amount)
      if (last.length > 0) {
        markovWords.addWord(first, last)
        markovWords.addWord(last, "\n")
      }
      else {
        markovWords.addWord(first, "\n")
      }
    }
    if (words.length - amount <= 0) {
      val all = getWordsFromLine(words, 0, amount)
      markovWords.addWord(all, "\n")
    }
  }

  def getWordsFromLine(words: Array[String], wordNum: Int, amount: Int): String = {
    var output = ""
    for(
      i <- wordNum until wordNum+amount
      if i < words.length
    ) {
      output += words(i) + " "
    }
    output
  }
}

//object Markov {
//  def main(args: Array[String]) : Unit = {
//    val markovParser = new MarkovParser("bijbel.txt")
//    val markovWords = markovParser.getWords(2)
//
//    var word = "\n"
//    var i = 100
//    while (i > 0) {
//      word = markovWords.getANextWord(word).get
//      if (word != "\n") print(word) else println()
//      if (word == "\n") i -= 1
//    }
//  }
//}