package Level01

object Problem0004 {
  private def isPalindrome(n: Long): Boolean =
    n == n.toString.reverse.toInt

  def largestPalindromeProduct(nrDigits: Int): Long =
    val first: Int = math.pow(10, nrDigits - 1).toInt
    val last: Int = 10 * first
    (first until last)
      .flatMap(a => (a until last).map(b => a.toLong * b))
      .filter(isPalindrome)
      .max

  def main(args: Array[String]): Unit =
    val d: Int = 3
    val result: Long = largestPalindromeProduct(d)
    println(result)
}
