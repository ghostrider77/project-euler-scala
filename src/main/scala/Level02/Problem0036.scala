package Level02

import scala.annotation.tailrec

object Problem0036 {
  def isPalindrome(n: Int, base: Int): Boolean =
    @tailrec
    def loop(acc: Int, k: Int): Int =
      if k == 0 then acc
      else loop(base*acc + k % base, k / base)

    val reversed: Int = loop(0, n)
    reversed == n

  private def sumOfDoublePalindromes(limit: Int): Int =
    def isDoublePalindrome(n: Int): Boolean =
      isPalindrome(n, 10) && isPalindrome(n, 2)

    (1 to limit by 2).foldLeft(0)((acc, n) => if isDoublePalindrome(n) then acc + n else acc)

  def main(args: Array[String]): Unit =
    val limit: Int = 1000000
    val result: Int = sumOfDoublePalindromes(limit)
    println(result)
}
