package io.github.rogern.scramble

import scala.annotation.tailrec

object RunPermutations extends App {
  private val text = "rogernordquist"

  Permutations(text, 3) match {
    case Left(value) => sys.error(value)
    case Right(value) =>
      println(value.size)
      value.foreach(println)
  }
}

object Permutations {

  def apply(text: String, splitCount: Int): Either[String, List[List[String]]] = {
    val length = text.length
    lazy val results =
        Combination(length, splitCount)
      .map(split(text, _))

    Either.cond(splitCount < length, results, "Can't have split count as long or longer than text.length")
  }

  private def split(text: String, indices: List[Int]): List[String] = {
    val points = indices.prepended(0).appended(text.length)
    points.sliding(2).collect {
      case h :: n :: Nil => text.substring(h, n)
    }.toList
  }

  def tokenize(text: String, splits: Int): List[String] = {
    val step = text.length / splits

    @tailrec
    def split(s: String, index: Int, r: List[String] = List.empty): List[String] = {
      if (s.length * 2 < index) r
      else {
        s.splitAt(index.min(s.length)) match {
          case (a, b) => split(b, index, r :+ a)
        }
      }
    }

    split(text, step)
  }
}
