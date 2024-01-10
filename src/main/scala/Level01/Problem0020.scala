package Level01

object Problem0020 {
  def factorialDigitSum(n: Int): Int =
    val factorial: BigInt = (1 to n).foldLeft(BigInt(1))((acc, k) => acc * BigInt(k))
    factorial.toString().foldLeft(0)((acc, char) => acc + char.asDigit)

  def main(args: Array[String]): Unit =
    val n: Int = 100
    val result: Int = factorialDigitSum(n: Int)
    println(result)
}
