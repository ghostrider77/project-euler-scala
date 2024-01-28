package Level02

import scala.annotation.tailrec

object Problem0028 {
  private def diagonalSum(start: Int, diff: Int, size: Int): Int =
    @tailrec
    def loop(acc: Int, value: Int, d: Int, level: Int): Int =
      if 2*level + 1 == size then acc
      else
        val nextValue: Int = value + d
        val nextD: Int = d + 8
        loop(acc + nextValue, nextValue, nextD, level + 1)

    loop(start, start, diff, 0)

  def calcDiagonalSums(size: Int): Int =
    List(2, 4, 6, 8).foldLeft(-3)((acc, diff) => acc + diagonalSum(1, diff, size))

  def main(args: Array[String]): Unit =
    val size: Int = 1001
    val result: Int = calcDiagonalSums(size)
    println(result)
}
