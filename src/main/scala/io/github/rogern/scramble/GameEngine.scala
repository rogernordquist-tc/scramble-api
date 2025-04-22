package io.github.rogern.scramble

import java.util
import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Random, Using}

class GameEngine(source: Source) {

  import GameEngine._

  val wordMap = Using(source) { file =>
    val tuples = for {
      word <- filterList(file.getLines().toList)
    } yield GameEngine.mkKey(word) -> word
    tuples.groupMap(_._1)(_._2)
  }.getOrElse(sys.error(s"Could not read file ${source.descr}"))

  @tailrec
  final def scrambleWords(words: Array[String], attempts: Int = 0, findMatchAttempt: Int = 0): Either[(String, Int), (Array[String], Int)] = {
    val noOfWords = words.length
    val rand = new Random(attempts)
    val scrambledChars = rand.shuffle(words.mkString)
    var count = findMatchAttempt

    @tailrec
    def findMatch(choices: List[String], matchesFound: List[String] = List.empty): Option[List[String]] = {
      count = count + 1
      choices match {
        case _ if matchesFound.size == noOfWords =>
          Some(matchesFound)
        case Nil =>
          None
        case head :: tail =>
          val key = GameEngine.mkKey(head)
          val matches = wordMap
            .get(key)
            .filterNot(_.forall(words.contains(_)))
            .flatMap(Random.shuffle(_).headOption)
            .toList
          findMatch(tail, matches ++ matchesFound)
      }
    }

    object Found {
      def unapply(arg: List[String]): Option[List[String]] = {
        findMatch(arg)
      }
    }

    Permutations(scrambledChars.toString, noOfWords) match {
      case Left(err) =>
        sys.error(err)
      case Right(value) =>
        val maybeFound = Random.shuffle(value)
          .collectFirst { case Found(r) => r }

        maybeFound match {
          case Some(list) => Right(list.toArray -> count)
          case _ if attempts < 150 => scrambleWords(words, attempts + 1, count)
          case _ => Left(s"No combinations found for ${words.mkString(" ")}" -> count)
        }
    }
  }
}
object GameEngine {

  val Locale = util.Locale.of("en", "US")
  val WordLengthCap = 10

  def mkKey(word: String): String = {
    word.toCharArray.sorted.mkString
  }

  private def filterList(list: List[String]): List[String] = {
    list
      .filterNot(_.length > WordLengthCap)
      .map(_.trim.toLowerCase(Locale))
      .distinct
  }
}
