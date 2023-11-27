package Level01

object Problem0016 {
  def powerDigitSum(n: Int): Int =
    val power: BigInt = BigInt(2).pow(n)
    power.toString.toCharArray.map(_.asDigit).sum

  def main(args: Array[String]): Unit =
    val n: Int = 1000
    val result: Int = powerDigitSum(n)
    println(result)
}
