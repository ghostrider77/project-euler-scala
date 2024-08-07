package Level04

import scala.annotation.tailrec

object Problem0095 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def findLargestExponent(n: Int, p: Int): (Int, Int) =
    @tailrec
    def loop(m: Int, alpha: Int): (Int, Int) =
      if m % p != 0 then (m, alpha)
      else loop(m / p, alpha + 1)

    loop(n, 0)

  private def calcPrimeFactorization(n: Int, primes: List[Int]): Map[Int, Int] =
    @tailrec
    def loop(acc: Map[Int, Int], m: Int, ps: List[Int]): Map[Int, Int] =
      if m == 1 then acc
      else ps match
        case Nil => acc + (m -> 1)
        case p :: pss =>
          findLargestExponent(m, p) match
            case (_, 0) => loop(acc, m, pss)
            case (rest, alpha) => loop(acc + (p -> alpha), rest, pss)

    loop(Map.empty[Int, Int], n, primes)

  private def calcSumOfDivisors(factorization: Map[Int, Int]): Long =
    factorization.foldLeft(1L){ case (acc, (p, alpha)) => acc * ((math.pow(p, alpha + 1).toLong - 1) / (p - 1)) }

  private def amicableChainLength(n: Int, divisorSums: Vector[Long], limit: Int): Int =
    @tailrec
    def loop(k: Int, visited: Set[Long]): Int =
      val next: Long = divisorSums(k)
      if next == n then visited.size
      else if next < n || next > limit || visited.contains(next) then 0
      else loop(next.toInt, visited + next)

    loop(n, Set(n))

  def longestAmicableChain(limit: Int): Int =
    val primes: List[Int] = 2 :: (3 to limit by 2).filter(isPrime).toList
    val properDivisorSum: Vector[Long] =
      Vector.tabulate(limit + 1){ k => if k <= 1 then 0 else calcSumOfDivisors(calcPrimeFactorization(k, primes)) - k }
    val (maxN, _): (Int, Int) = (2 to limit).foldLeft((0, 0)){
      case (acc @ (_, maxSize), n) =>
        val size: Int = amicableChainLength(n, properDivisorSum, limit)
        if size > maxSize then (n, size) else acc
    }
    maxN

  def main(args: Array[String]): Unit =
    val limit: Int = 1000000
    val result: Int = longestAmicableChain(limit)
    println(result)
}
