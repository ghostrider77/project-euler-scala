package Level02

import scala.annotation.tailrec

object Problem0030 {
  private def powerDigitSum(n: Int, exponent: Int): Int =
    @tailrec
    def loop(acc: Int, k: Int): Int =
      if k == 0 then acc
      else loop(acc + math.pow(k % 10, exponent).toInt, k / 10)

    loop(0, n)

  def calcSumOfDigitPowers(exponent: Int): Int =
    val limit: Int = 6 * math.pow(9, exponent).toInt
    (10 to limit).foldLeft(0)((acc, n) => if n == powerDigitSum(n, exponent) then acc + n else acc)

  def main(args: Array[String]): Unit =
    val exponent: Int = 5
    val result: Int = calcSumOfDigitPowers(exponent)
    println(result)
}
