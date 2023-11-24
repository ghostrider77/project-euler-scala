package Level01

object Problem0010 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  def sumOfPrimes(n: Int): Long =
    (2 until n).filter(isPrime).map(_.toLong).sum

  def main(args: Array[String]): Unit =
    val n: Int = 2_000_000
    val result: Long = sumOfPrimes(n)
    println(result)
}
