package io.github.rogern.scramble

import java.util.Locale

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Random, Using}
import scala.io.StdIn.readLine

object WordTransformer extends App {
  val Locale = new Locale("se", "sv")
  val WordLengthCap = 10

  if (args.length != 1) {
    println("WordTransformer takes a single argument 'filename' with words, exiting")
  } else {
    var count = 0
    val fileName = args(0)
    val wordMap = Using(Source.fromFile(fileName)) { file =>
      val tuples = for {
        word <- filterList(file.getLines().toList)
      } yield mkKey(word) -> word

      tuples.groupMap(_._1)(_._2)
    }.getOrElse(sys.error(s"Could not read file $fileName"))

    println("Welcome to words scrambler!")
    println(s"Using a wordlist of [${wordMap.size}] words.")
    println("Enter words in a single line and press enter")
    run()

    def run(): Unit = {
      print("> ")
      Option(readLine()) match {
        case Some(line) if line.replaceAll(" ", "").length > WordLengthCap * 2 =>
          println(s"Please enter less characters than ${WordLengthCap * 2}")
          run()
        case Some(line) if line.trim != ":q" =>
          println(line)
          scrambleWords(line.toLowerCase(Locale).split(" ")) match {
            case Left(errMessage) =>
              println(s"$errMessage, please try another")
            case Right(words) =>
              println(words.mkString(" "))
          }
          println(s"Attempts $count")
          count = 0
          run()
        case _ =>
          println("Exiting")
      }
    }

    @tailrec
    def scrambleWords(words: Array[String], attempts: Int = 0): Either[String, Array[String]] = {
      val noOfWords = words.length
      val rand = new Random(attempts)
      val scrambledChars = rand.shuffle(words.mkString)

      @tailrec
      def findMatch(choices: List[String], matchesFound: List[String] = List.empty): Option[List[String]] = {
        count = count + 1
        choices match {
          case _ if matchesFound.size == noOfWords =>
            Some(matchesFound)
          case Nil =>
            None
          case head :: tail =>
            val key = mkKey(head)
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
            case Some(list) => Right(list.toArray)
            case _ if attempts < 150 => scrambleWords(words, attempts + 1)
            case _ => Left("No combinations found")
          }
      }
    }
  }
  private def filterList(list: List[String]): List[String] = {
    list
      .filterNot(_.length > WordLengthCap)
      .map(_.trim.toLowerCase(Locale))
      .distinct
  }

  private def mkKey(word: String): String = {
    word.toCharArray.sorted.mkString
  }
}
