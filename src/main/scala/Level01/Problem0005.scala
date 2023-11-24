package Level01

import scala.annotation.tailrec

object Problem0005 {
  @tailrec
  private def calcGCD(a: Int, b: Int): Int =
    if a % b == 0 then b else calcGCD(b, a % b)

  private def calcLCM(a: Int, b: Int): Int =
    val gcd: Int = calcGCD(a, b)
    (a / gcd) * b

  def calcSmallestMultiple(limit: Int): Int =
    (2 to limit).foldLeft(1)(calcLCM)

  def main(args: Array[String]): Unit =
    val limit: Int = 20
    val result: Int = calcSmallestMultiple(limit)
    println(result)
}
