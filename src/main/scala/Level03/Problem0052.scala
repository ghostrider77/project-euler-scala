package Level03

import scala.annotation.tailrec

object Problem0052 {
  private def permutedMultiples(maxK: Int): Int =
    def calcSortedDigits(n: Int): Array[Int] = n.toString.toCharArray.map(_.asDigit).sorted

    @tailrec
    def loop(n: Int): Int =
      val digits: Array[Int] = calcSortedDigits(n)
      if (2 to maxK).forall{ k => digits.sameElements(calcSortedDigits(k * n)) } then n
      else loop(n + 1)

    loop(1)

  def main(args: Array[String]): Unit =
    val maxK: Int = 6
    val result: Int = permutedMultiples(maxK)
    println(result)
}
