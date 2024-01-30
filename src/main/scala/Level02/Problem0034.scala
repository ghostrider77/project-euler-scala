package Level02

object Problem0034 {
  private val Factorials: Vector[Int] = (1 to 9).scanLeft(1)(_ * _).toVector

  def isFactorialSumOfDigits(n: Int): Boolean =
    val digitFactorialSum: Int = n.toString.foldLeft(0)((acc, chr) => acc + Factorials(chr.asDigit))
    digitFactorialSum == n

  private def calcDigitFactorialSums(limit: Int): Int =
    (10 until limit).foldLeft(0)((acc, n) => if isFactorialSumOfDigits(n) then acc + n else acc)

  def main(args: Array[String]): Unit =
    val limit: Int = 100000
    val result: Int = calcDigitFactorialSums(limit)
    println(result)
}
