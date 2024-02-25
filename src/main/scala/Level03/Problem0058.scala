package Level03

import scala.annotation.tailrec

object Problem0058 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  def diagonalPrimes(primeRatio: Double): Int =
    @tailrec
    def loop(nrPrimes: Int, level: Int, diagonalValues: List[Int], differences: List[Int]): Int =
      val sideLength: Int = 2*level + 1
      val nrDiagonalElements: Double = 4*level + 1
      if level >= 1 && (nrPrimes / nrDiagonalElements) < primeRatio then sideLength
      else
        val nextDiagonalValues: List[Int] = diagonalValues.lazyZip(differences).map(_ + _)
        val nextDifferences: List[Int] = differences.map(_ + 8)
        loop(nrPrimes + nextDiagonalValues.count(isPrime), level + 1, nextDiagonalValues, nextDifferences)

    loop(0, 0, List(1, 1, 1, 1), List(2, 4, 6, 8))

  def main(args: Array[String]): Unit =
    val ratio: Double = 0.1
    val result: Int = diagonalPrimes(ratio)
    println(result)
}
