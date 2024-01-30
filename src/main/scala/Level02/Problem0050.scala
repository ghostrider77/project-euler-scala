package Level02

import scala.annotation.tailrec

object Problem0050 {
  private def generatePrimes(limit: Int): List[Int] =
    val primes: Array[Boolean] = Array.tabulate(limit + 1)(k => k >= 2)
    (2 to math.sqrt(limit).toInt).foreach { p =>
      if primes(p) then
        (p * p to limit by p).foreach { k => primes(k) = false }
    }
    primes.iterator.zipWithIndex.collect { case (isPrime, p) if isPrime => p }.toList

  def sumOfConsecutivePrimes(limit: Int): Int =
    val primes: List[Int] = generatePrimes(limit)
    val primeSet: Set[Int] = primes.toSet

    @tailrec
    def loop(primeSum: Int, maxLength: Int, primes: List[Int]): (Int, Int) = primes match
      case Nil => (primeSum, maxLength)
      case p :: ps =>
        val cumulativeSums: List[Int] = ps.iterator.scanLeft(p)(_ + _).takeWhile(_ <= limit).toList
        val (s, length): (Int, Int) =
          cumulativeSums.zipWithIndex.findLast{ case (sum, _) => primeSet.contains(sum) } match
            case None => throw Exception("The sequence should contain at least one prime.")
            case Some(s, ix) => (s, ix + 1)
        if length <= maxLength then loop(primeSum, maxLength, ps)
        else loop(s, length, ps)

    val (largestPrime, _): (Int, Int) = loop(0, 0, primes)
    largestPrime

  def main(args: Array[String]): Unit =
    val limit: Int = 1000000
    val result: Int = sumOfConsecutivePrimes(limit)
    println(result)
}
