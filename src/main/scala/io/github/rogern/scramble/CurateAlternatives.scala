package io.github.rogern.scramble

import com.typesafe.config.ConfigFactory

import java.io.{File, PrintWriter}
import java.net.URI
import java.util.Scanner
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

object CurateAlternatives extends App {

  private val config = ConfigFactory.load("application")
  private val alternativesPath = config.getString("scramble-api.alternatives")
  private val wordsPath = config.getString("scramble-api.words")

  private val ge = Using(Source.fromURI(URI.create(wordsPath))) { uri =>
    new GameEngine(uri)
  }.getOrElse(sys.error(s"Could not read words file $wordsPath"))

  private val alternatives = Using(Source.fromURI(URI.create(alternativesPath))) { uri =>
    uri.getLines().toSet.toSeq
  }.getOrElse(sys.error(s"Could not read alternatives file $alternativesPath"))

  private val (notOk, ok) = alternatives.partitionMap { alt =>
    ge.scrambleWords(alt.toLowerCase(GameEngine.Locale).split(" ")).map(_ => alt)
  }
  notOk.foreach { case (notOkMsg, _) =>
    println(notOkMsg)
  }

  if (notOk.nonEmpty) {
    var choice = ""
    val scanner = new Scanner(System.in)
    println("Write ok results to new file? (y/N)")
    while (true) {
      choice = scanner.nextLine()
      if (choice.trim.toLowerCase == "y") {
        while (true) {
          println("Enter filename:")
          val filename = scanner.nextLine()
          Try(new File(filename).createNewFile())
            .filter(identity)
            .foreach { _ =>
              Using(new PrintWriter(filename)) { writer =>
                ok.foreach { alt =>
                  writer.println(alt)
                }
              } match {
                case Success(_) =>
                  println(s"File $filename created successfully.")
                  sys.exit(0)
                case Failure(exception) =>
                  println(s"Failed to create file: ${exception.getMessage}")
              }
            }
        }
      } else if (choice.trim.toLowerCase == "n") {
        println("Exiting...")
        sys.exit(0)
      } else {
        println("Please enter 'y' or 'N'")
      }
    }
  }
  else {
    println("All alternatives are ok, done.")
  }
}
