package Level02

import scala.annotation.tailrec

object Problem0037 {
  private val NrTruncatablePrimes: Int = 11

  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def isTruncatablePrime(p: Int, primes: Set[Int]): Boolean =
    def allPartsArePrime(ps: Iterator[String]): Boolean =
      ps.filter(_.nonEmpty).map(_.toInt).forall(primes.contains)

    val s: String = p.toString
    p >= 10 && allPartsArePrime(s.tails) && allPartsArePrime(s.inits)

  private def calcSumOfTruncatablePrimes(): Int =
    @tailrec
    def loop(acc: Int, count: Int, primes: Set[Int], n: Int): Int =
      if count == NrTruncatablePrimes then acc
      else if isPrime(n) then
        val updatedPrimes: Set[Int] = primes + n
        if isTruncatablePrime(n, updatedPrimes) then loop(acc + n, count + 1, updatedPrimes, n + 2)
        else loop(acc, count, updatedPrimes, n + 2)
      else loop(acc, count, primes, n + 2)

    loop(0, 0, Set(2), 3)

  def main(args: Array[String]): Unit =
    val result: Int = calcSumOfTruncatablePrimes()
    println(result)
}
