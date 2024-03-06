package Level03

import scala.annotation.tailrec

object Problem0073 {
  @tailrec
  private def calcGcd(a: Int, b: Int): Int =
    if b == 0 then a
    else calcGcd(b, a % b)

  def calcNrReducedFractionsInRange(denominatorBound: Int): Int =
    def nrFractionsWithGivenDenominator(d: Int): Int =
      val nMin: Int = d / 3 + 1
      val nMax: Int = d / 2
      (nMin to nMax).count(calcGcd(_, d) == 1)

    (4 to denominatorBound).foldLeft(0)((acc, denom) => acc + nrFractionsWithGivenDenominator(denom))

  def main(args: Array[String]): Unit =
    val denominatorBound: Int = 12000
    val result: Int = calcNrReducedFractionsInRange(denominatorBound)
    println(result)
}
