package Level03

import scala.annotation.tailrec

object Problem0060 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def getNextPrime(n: Int): Int =
    Iterator.from(n + 1).filter(isPrime).next()

  private def isCompatible(p: Int, q: Int): Boolean =
    val ps: String = p.toString
    val qs: String = q.toString
    isPrime((ps + qs).toInt) && isPrime((qs + ps).toInt)

  private def collectCompatiblePrimes(n: Int, primes: List[Int], p: Int): List[List[Int]] =
    primes
      .filter(isCompatible(p, _))
      .combinations(n - 1)
      .filter(_.combinations(2).collect{ case List(a, b) => (a, b) }.forall(isCompatible))
      .map(cs => p :: cs)
      .toList

  def findPrimePairSetWithLowestSum(n: Int): Int =
    @tailrec
    def loop(primes: List[Int], p: Int): Int =
      val compatiblePrimes: List[List[Int]] = collectCompatiblePrimes(n, primes, p)
      compatiblePrimes.map(_.sum).minOption match
        case None => loop(p :: primes, getNextPrime(p))
        case Some(result) => result

    val initialPrimes: List[Int] = Iterator.from(3).filter(isPrime).take(n - 1).toList
    val nextPrime: Int = getNextPrime(initialPrimes.last)
    loop(initialPrimes, nextPrime)

  def main(args: Array[String]): Unit =
    val n: Int = 5
    val result: Int = findPrimePairSetWithLowestSum(n)
    println(result)
}
