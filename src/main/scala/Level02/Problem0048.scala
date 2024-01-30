package Level02

object Problem0048 {
  private val Modulus: Long = 10000000000L

  def lastDigitOfSelfPowerSum(limit: Int): String =
    def selfPower(n: Int): Long =
      (1 to n).foldLeft(1L)((acc, _) => (acc * n) % Modulus)

    val sum: Long = (1 to limit).foldLeft(0L)((acc, n) => (acc + selfPower(n)) % Modulus)
    f"$sum%010d"

  def main(args: Array[String]): Unit =
    val limit: Int = 10
    val result: String = lastDigitOfSelfPowerSum(limit)
    println(result)
}
