package Level03

import scala.annotation.tailrec

object Problem0062 {
  def findSmallestCube(nrPermutations: Int): Long =
    @tailrec
    def loop(acc: Map[String, List[Int]], n: Int): Long =
      val sortedCubeString: String = math.pow(n, 3).toLong.toString.sorted
      val nValues: List[Int] = n :: acc.getOrElse(sortedCubeString, Nil)
      if nValues.length == nrPermutations then math.pow(nValues.min, 3).toLong
      else loop(acc.updated(sortedCubeString, nValues), n + 1)

    loop(Map(), 1)

  def main(args: Array[String]): Unit =
    val nrPermutations: Int = 5
    val result: Long = findSmallestCube(nrPermutations)
    println(result)
}
