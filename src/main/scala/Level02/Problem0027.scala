package Level02

import scala.annotation.tailrec

object Problem0027 {
  private class QuadraticPolynomial(val a: Int, val b: Int):
    def apply(x: Int): Int = x * x + a * x + b

  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def primeRunLength(p: QuadraticPolynomial): Int =
    @tailrec
    def loop(n: Int): Int =
      if isPrime(math.abs(p(n))) then loop(n + 1)
      else n

    loop(0)

  def calcPolynomicalWithLongestPrimeRun(limit: Int): Int =
    val polynomials: Iterator[QuadraticPolynomial] =
      (-limit + 1 until limit)
        .iterator
        .flatMap(a => (-limit to limit).map(b => QuadraticPolynomial(a, b)))
    val (_, maxProduct): (Int, Int) = polynomials.foldLeft((0, 0)){
      case (acc @ (maxLength, product), p) =>
        val runLength: Int = primeRunLength(p)
        if runLength > maxLength then (runLength, p.a * p.b)
        else acc
    }
    maxProduct

  def main(args: Array[String]): Unit =
    val limit: Int = 1000
    val result: Int = calcPolynomicalWithLongestPrimeRun(limit)
    println(result)
}
