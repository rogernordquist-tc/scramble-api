package io.github.rogern.scramble

import com.typesafe.config.ConfigFactory
import upickle.default._

import java.net.URI
import scala.io.Source
import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps

object GameServer extends cask.MainRoutes {

  case class Scramble(scramble: String, rightAnswer: String)

  object Scramble {
    implicit val rw: ReadWriter[Scramble] = macroRW
  }

  @cask.getJson("/scramble")
  def giveScramble() = {
    def run = (for {
      alt <- rand.shuffle(alternatives).headOption.toRight("No alternatives found").left.map(_ -> 0)
      result <- ge.scrambleWords(alt.trim.split(" "))
      (scrambledWords, _) = result
    } yield Scramble(scrambledWords.mkString(" "), alt)).tap { res =>
      res.foreach {
        case Scramble(scramble, rightAnswer) =>
          if (logResponse) {
            println(s"Scramble: $scramble, Right Answer: $rightAnswer")
          }
      }
    }

    def logErr(leftContent: (String, Int)) = {
      val (err, findMatchCount) = leftContent
      println(s"tried $findMatchCount times to find match and got error: $err")
    }

    run
      .left
      .flatMap { leftContent =>
        logErr(leftContent)
        run
      }
      .fold(
        leftContent => {
          logErr(leftContent)
          throw new IllegalStateException(s"Error: ${leftContent._1}")
        },
        identity
      )
  }

  val config = ConfigFactory.load("application")
  val alternativesPath = config.getString("scramble-api.alternatives")
  val wordsPath = config.getString("scramble-api.words")
  val logResponse = config.getBoolean("scramble-api.log-response")

  val ge = Using(Source.fromURI(URI.create(wordsPath))) { uri =>
    new GameEngine(uri)
  }.getOrElse(sys.error(s"Could not read words file $wordsPath"))

  val alternatives = Using(Source.fromURI(URI.create(alternativesPath))) { uri =>
    uri.getLines().toSeq.map(_.toLowerCase(GameEngine.Locale))
  }.getOrElse(sys.error(s"Could not read alternatives file $alternativesPath"))

  val rand = new scala.util.Random()

  initialize()
  println("Starting server...")
}
