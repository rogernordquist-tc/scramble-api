package io.github.rogern.scramble

import scala.io.Source
import scala.io.StdIn.readLine

object WordTransformer extends App {

  if (args.length != 1) {
    println("WordTransformer takes a single argument 'filename' with words, exiting")
  } else {
    val fileName = args(0)
    val gameEngine = new GameEngine(Source.fromFile(fileName))
    println("Welcome to words scrambler!")
    println(s"Using a wordlist of [${gameEngine.wordMap.size}] words.")
    println("Enter words in a single line and press enter")
    run()

    def run(): Unit = {
      print("> ")
      Option(readLine()) match {
        case Some(line) if line.replaceAll(" ", "").length > GameEngine.WordLengthCap * 2 =>
          println(s"Please enter less characters than ${GameEngine.WordLengthCap * 2}")
          run()
        case Some(line) if line.trim != ":q" =>
          println(line)
          gameEngine.scrambleWords(line.toLowerCase(GameEngine.Locale).split(" ")) match {
            case Left((errMessage, count)) =>
              println(s"$errMessage, please try another")
              println(s"Attempts $count")
            case Right((words, count)) =>
              println(words.mkString(" "))
              println(s"Attempts $count")
          }
          run()
        case _ =>
          println("Exiting")
      }
    }
  }
}
