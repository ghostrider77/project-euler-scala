package Level03

import scala.annotation.tailrec

object Problem0064 {
  private def getSquareRootExpansion(n: Int): List[Int] =
    val a0: Int = math.sqrt(n).toInt

    @tailrec
    def loop(acc: List[(Int, Int, Int)], m: Int, d: Int, a: Int): List[Int] =
      if acc.contains((m, d, a)) then acc.reverseIterator.map{ case (_, _, a) => a }.drop(1).toList
      else
        val m1: Int = d*a - m
        val d1: Int = (n - m1*m1) / d
        val a1: Int = (a0 + m1) / d1
        loop((m, d, a) :: acc, m1, d1, a1)

    if a0 * a0 == n then Nil
    else loop(Nil, 0, 1, a0)

  def calcNrOfOddPeriodSquareRootConvergents(limit: Int): Int =
    (1 to limit).count(n => getSquareRootExpansion(n).length % 2 == 1)

  def main(args: Array[String]): Unit =
    val limit: Int = 10000
    val result: Int = calcNrOfOddPeriodSquareRootConvergents(limit)
    println(result)
}
