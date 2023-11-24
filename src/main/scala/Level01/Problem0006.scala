package Level01

object Problem0006 {
  def sumSquareDifference(n: Int): Int =
    val sum: Int = (1 to n).sum
    val squareSum: Int = (1 to n).map(k => k * k).sum
    sum * sum - squareSum

  def main(args: Array[String]): Unit =
    val n: Int = 100
    val result: Int = sumSquareDifference(n)
    println(result)
}
