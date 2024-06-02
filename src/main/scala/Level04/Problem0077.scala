package Level04

import scala.annotation.tailrec

object Problem0077 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def primeSums(k: Int, primes: List[Int]): Int =
    if k == 0 then 1
    else primes match
      case Nil => 0
      case p :: rest => (k to 0 by -p).foldLeft(0)((acc, m) => acc + primeSums(m, rest))

  def primeSummation(limit: Int): Int =
    @tailrec
    def loop(n: Int, primes: List[Int]): Int =
      val currentPrimes: List[Int] = if isPrime(n) then n :: primes else primes
      val nrWays: Int = primeSums(n, currentPrimes)
      if nrWays > limit then n
      else loop(n + 1, currentPrimes)

    loop(2, Nil)

  def main(args: Array[String]): Unit =
    val limit: Int = 5000
    val result: Int = primeSummation(limit)
    println(result)
}
