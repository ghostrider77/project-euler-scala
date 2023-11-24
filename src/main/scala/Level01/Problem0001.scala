package Level01

object Problem0001 {
  def calcSumOfMultiples(n: Int): Int =
    (1 until n).filter(k => k % 3 == 0 || k % 5 == 0).sum

  def main(args: Array[String]): Unit =
    val n: Int = 1000
    val result: Int = calcSumOfMultiples(n)
    println(result)
}
