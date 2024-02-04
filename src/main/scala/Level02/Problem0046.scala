package Level02

import scala.annotation.tailrec

object Problem0046 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def findGoldbachsCounterExample(): Int =
    @tailrec
    def loop(primes: Set[Int], n: Int): Int =
      if isPrime(n) then loop(primes + n, n + 2)
      else if Iterator.from(1).map(k => n - 2 * k * k).takeWhile(_ > 1).exists(primes.contains) then loop(primes, n + 2)
      else n

    loop(Set(2), 3)

  def main(args: Array[String]): Unit =
    val result: Int = findGoldbachsCounterExample()
    println(result)
}
