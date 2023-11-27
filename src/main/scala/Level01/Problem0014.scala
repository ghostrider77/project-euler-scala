package Level01

import scala.annotation.tailrec

object Problem0014 {
  def collatzSequenceLength(start: Int): Int =
    @tailrec
    def loop(n: Long, length: Int): Int =
      if n == 1 then length
      else if n % 2 == 0 then loop(n / 2, length + 1)
      else loop(3*n + 1, length + 1)

    loop(start, 1)

  def findLongestCollatzSequence(limit: Int): Int =
    val (maxN, _): (Int, Int) = (2 until limit).foldLeft((1, 1)){
      case (acc @ (_, maxLength), n) =>
        val length: Int = collatzSequenceLength(n)
        if length > maxLength then (n, length) else acc
    }
    maxN

  def main(args: Array[String]): Unit =
    val limit: Int = 1_000_000
    val result: Int = findLongestCollatzSequence(limit)
    println(result)
}
