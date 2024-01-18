package Level01

import scala.annotation.tailrec
import scala.math.Integral.Implicits.infixIntegralOps

object Problem0024 {
  private def factorial(n: Int): Int =
    (1 to n).product

  def calcNthPermutation(n: Int, allDigits: List[Int]): List[Int] =
    @tailrec
    def loop(permutation: List[Int], availableDigits: List[Int], limit: Int): List[Int] = availableDigits match
      case Nil => permutation.reverse
      case _ =>
        val fact: Int = factorial(availableDigits.length - 1)
        val (k, r): (Int, Int) = limit /% fact
        val digit: Int = availableDigits(k)
        val digits: List[Int] =
          availableDigits.zipWithIndex.filterNot{ case (_, ix) => ix == k }.map{ case (d, _) => d }
        loop(digit :: permutation, digits, r)

    loop(Nil, allDigits, n - 1)

  def main(args: Array[String]): Unit =
    val n: Int = 1000000
    val allDigits: List[Int] = (0 to 9).toList
    val result: List[Int] = calcNthPermutation(n, allDigits)
    println(result.mkString)
}
