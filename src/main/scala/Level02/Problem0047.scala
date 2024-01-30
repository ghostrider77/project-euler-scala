package Level02

import scala.annotation.tailrec

object Problem0047 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  def distinctPrimeFactors(k: Int): Int =
    val primes: LazyList[Int] = LazyList.from(2).filter(isPrime)

    @tailrec
    def removeFactor(n: Int, p: Int): Int =
      if n % p == 0 then removeFactor(n / p, p) else n

    def hasKFactors(n: Int, k: Int): Boolean =
      @tailrec
      def loop(m: Int, nrPrimeFactors: Int, ps: LazyList[Int]): Boolean =
        if nrPrimeFactors >= k then true
        else if m <= 1 then false
        else ps match
          case p #:: pss =>
            if m % p == 0 then loop(removeFactor(m, p), nrPrimeFactors + 1, pss)
            else loop(m, nrPrimeFactors, pss)
          case _ => throw Exception("No more prime numbers.")
      loop(n, 0, primes)

    Iterator.from(0).map(hasKFactors(_, k)).sliding(k).zipWithIndex.find{ case (seq, _) => seq.forall(identity) } match
      case Some((_, n)) => n
      case _ => throw Exception("No such set of consecutive numbers have found.")

  def main(args: Array[String]): Unit =
    val k: Int = 4
    val result: Int = distinctPrimeFactors(k)
    println(result)
}
