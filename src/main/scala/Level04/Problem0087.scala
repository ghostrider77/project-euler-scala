package Level04

object Problem0087 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  def numberOfPrimePowerSums(n: Int): Int =
    val limit: Int = math.sqrt(n).toInt
    val primes: List[Int] = 2 :: (3 to limit by 2).filter(isPrime).toList
    val primePowerSums: Set[Long] = for {
      p2 <- primes.iterator.map(p => p * p).toSet
      p3 <- primes.iterator.map(p => math.pow(p, 3).toLong).filter(_ < n)
      p4 <- primes.iterator.map(p => math.pow(p, 4).toLong).filter(_ < n)
      if p2 + p3 + p4 < n
    } yield p2 + p3 + p4
    primePowerSums.size

  def main(args: Array[String]): Unit =
    val limit: Int = 50000000
    val result: Int = numberOfPrimePowerSums(limit)
    println(result)
}
