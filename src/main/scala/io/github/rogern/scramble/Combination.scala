package io.github.rogern.scramble

object RunCombination extends App {
  Combination(14, 3).foreach(println)
}

object Combination {

  def combinationUtil(
    arr: Array[Int],
    data: Array[Int],
    start: Int,
    end: Int,
    index: Int,
    splitCount: Int,
    result: List[List[Int]] = List.empty
  ): List[List[Int]] = {

    if (index == splitCount) {
      result :+ (0 until splitCount)
        .map(data(_))
        .toList
    } else {
      (start to end)
        .filter(x => end - x + 1 >= splitCount - index)
        .foldLeft(result) { (list, i) =>
          data(index) = arr(i)
          combinationUtil(arr, data, i + 1, end, index + 1, splitCount, list)
        }
    }
  }

  def apply(length: Int, splitCount: Int): List[List[Int]] = {
    val arr = (1 to length).toArray
    val data = new Array[Int](splitCount)
    combinationUtil(arr, data, 0, length - 2, 0, splitCount - 1)
  }
}
