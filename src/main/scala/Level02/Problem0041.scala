package Level02

object Problem0041 {
  private val Digits: List[Int] = (1 to 9).toList

  private def isPandigital(n: Int): Boolean =
    Digits.startsWith(n.toString.map(_.asDigit).sorted)

  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  def largestPandigitalPrime(limit: Int): Int =
    (3 to limit by 2).iterator.filter(n => isPrime(n) && isPandigital(n)).maxOption match
      case None => throw Exception("No pandigital prime was found.")
      case Some(p) => p

  def main(args: Array[String]): Unit =
    val limit: Int = 7654321
    val result: Int = largestPandigitalPrime(limit)
    println(result)
}
