package Level02

object Problem0035 {
  private def generatePrimes(limit: Int): Set[Int] =
    val primes: Array[Boolean] = Array.tabulate(limit + 1)(k => k >= 2)
    (2 to math.sqrt(limit).toInt).foreach{ p =>
      if primes(p) then
        (p*p to limit by p).foreach{ k => primes(k) = false }
    }
    primes.iterator.zipWithIndex.collect{ case (isPrime, p) if isPrime => p }.toSet

  private def generateShiftedNumbers(n: Int): List[Int] =
    val s: String = n.toString
    val length: Int = s.length
    val doubleString: String = s + s
    (0 until length).map(k => doubleString.slice(k, k+length).toInt).toList

  def calcNumberOfCircularPrimes(limit: Int): Int =
    val primes: Set[Int] = generatePrimes(limit)
    primes.count(p => generateShiftedNumbers(p).forall(primes.contains))

  def main(args: Array[String]): Unit =
    val limit: Int = 1000000
    val result: Int = calcNumberOfCircularPrimes(limit)
    println(result)
}
