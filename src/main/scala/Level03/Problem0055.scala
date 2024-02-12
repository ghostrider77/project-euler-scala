package Level03

import scala.annotation.tailrec

object Problem0055 {
  private val MaxIteration: Int = 50

  private def isPalindrome(n: BigInt): Boolean =
    n.toString == n.toString.reverse

  def isLychrelNumber(n: Int): Boolean =
    @tailrec
    def loop(m: BigInt, k: Int): Boolean =
      if k >= MaxIteration then true
      else
        val reversed: BigInt = BigInt(m.toString.reverse)
        val sum: BigInt = m + reversed
        if isPalindrome(sum) then false else loop(sum, k + 1)

    loop(BigInt(n), 0)

  private def calcNumberOfLychrelNumbers(limit: Int): Int =
    (0 to limit).count(isLychrelNumber)

  def main(args: Array[String]): Unit =
    val limit: Int = 10000
    val result: Int = calcNumberOfLychrelNumbers(limit)
    println(result)
}
