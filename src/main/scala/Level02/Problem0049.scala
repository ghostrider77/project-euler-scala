package Level02

import scala.annotation.tailrec

object Problem0049 {
  private val Number: String = "148748178147"

  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def isPermutation(p: Int, k: Int): Boolean =
    val orderedDigitsOfP: Array[Char] = p.toString.toCharArray.sorted
    val orderedDigitsOfK: Array[Char] = k.toString.toCharArray.sorted
    orderedDigitsOfP.sameElements(orderedDigitsOfK)

  private def digitsOfPrimePermutations(): String =
    val (nMin, nMax): (Int, Int) = (1000, 9999)
    val primes: Set[Int] = (nMin to nMax).filter(isPrime).toSet

    @tailrec
    def loop(p: Int, d: Int): Option[String] =
      val (left, right): (Int, Int) = (p - d, p + d)
      if left < nMin || right > nMax then None
      else if primes.contains(left) && primes.contains(right) && isPermutation(p, left) && isPermutation(p, right) then
        val concatenatedTerms: String = s"$left$p$right"
        if concatenatedTerms == Number then loop(p, d + 2)
        else Some(concatenatedTerms)
      else loop(p, d + 2)

    primes.iterator.flatMap(loop(_, 2)).nextOption() match
      case Some(result) => result
      case None => throw Exception("Prime permutation arithmetic sequence not found.")

  def main(args: Array[String]): Unit =
    val result: String = digitsOfPrimePermutations()
    println(result)
}
