package Level02

import scala.annotation.tailrec

object Problem0033 {
  @tailrec
  private def calcGcd(a: Int, b: Int): Int =
    if b == 0 then a
    else calcGcd(b, a % b)

  private def digitCancellingFractions(): Int =
    val curiousFractions: List[(Int, Int)] =
      (for {
        a <- 1 to 9
        b <- 1 to 9
        c <- 1 to 9
        if b != c && c*(10*a + b) == (10*b + c)*a && 10*a + b < 10*b + c
      } yield (10*a + b, 10*b + c)).toList
    val (num, denom): (Int, Int) = curiousFractions.foldLeft((1, 1)){ case ((a, b), (x, y)) => (a*x, b*y) }
    denom / calcGcd(num, denom)

  def main(args: Array[String]): Unit =
    val result: Int = digitCancellingFractions()
    println(result)
}
