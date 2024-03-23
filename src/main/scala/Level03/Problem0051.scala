package Level03

import scala.annotation.tailrec

object Problem0051 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def replaceDigits(p: String, mask: List[Int]): List[String] =
    val digits: List[Char] = ('0' to '9').toList
    digits.map(d => p.zipWithIndex.map((char, ix) => if mask.contains(ix) then d else char).mkString)

  private def getSmallestPrimeOfFamily(primes: List[Int], nrDigits: Int, familySize: Int): Option[Int] =
    val primeStrings: Set[String] = primes.map(_.toString).toSet
    val digitMasks: List[List[Int]] =
      (1 to nrDigits).iterator.flatMap(k => (0 until nrDigits).toList.combinations(k)).toList

    @tailrec
    def loop(ps: List[Int]): Option[Int] = ps match
      case Nil => None
      case p :: rest =>
        val primeString: String = p.toString
        val primesInFamily: List[Int] =
          digitMasks
            .map(replaceDigits(primeString, _).filter(primeStrings.contains))
            .filter(_.length == familySize)
            .flatten
            .map(_.toInt)
        primesInFamily.minOption match
          case None => loop(rest)
          case Some(prime) => Some(prime)

    loop(primes)

  def smallestPrimeFromASameDigitReplacementFamily(familySize: Int): Int =
    @tailrec
    def loop(n: Int): Int =
      val d: Int = n - 1
      val a: Int = math.pow(10, d).toInt
      val b: Int = 10 * a
      val primes: List[Int] = (a until b).filter(isPrime).toList
      getSmallestPrimeOfFamily(primes, d, familySize) match
        case None => loop(n + 1)
        case Some(p) => p

    loop(n = 1)

  def main(args: Array[String]): Unit =
    val familySize: Int = 8
    val result: Int = smallestPrimeFromASameDigitReplacementFamily(familySize)
    println(result)
}
