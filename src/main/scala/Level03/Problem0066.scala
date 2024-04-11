package Level03

import scala.annotation.tailrec

object Problem0066 {
  private case class Convergent(x: BigInt, y: BigInt)

  private def calcFundamentalSolution(n: Int): Option[Convergent] =
    val a0: Int = math.sqrt(n).toInt
    def advanceTriples(m: Int, d: Int, a: Int): (Int, Int, Int) =
      val m1: Int = d*a - m
      val d1: Int = (n - m1 * m1) / d
      val a1: Int = (a0 + m1) / d1
      (m1, d1, a1)

    @tailrec
    def loop(difference: BigInt, m: Int, d: Int, a: Int, c0: Convergent, c1: Convergent): Option[Convergent] =
      val Convergent(x0, y0) = c0
      val Convergent(x1, y1) = c1
      if difference == 1 then Some(c1)
      else
        val (m1, d1, a1) = advanceTriples(m, d, a)
        val c2 @ Convergent(x2, y2) = Convergent(a1*x1 + x0, a1*y1 + y0)
        val s: BigInt = x2*x2 - n*y2*y2
        loop(s, m1, d1, a1, c1, c2)

    if a0*a0 == n then None
    else
      val (m1, d1, a1): (Int, Int, Int) = advanceTriples(0, 1, a0)
      val c0 = Convergent(a0, 1)
      val c1 @ Convergent(x1, y1) = Convergent(a0 * a1 + 1, a1)
      val difference: BigInt = x1*x1 - n*y1*y1
      loop(difference, m1, d1, a1, c0, c1)

  def findMaximalFundamentalSolution(limit: Int): Int =
    (for {
      n <- 1 to limit
      c <- calcFundamentalSolution(n)
    } yield (n, c)).maxBy{ case (_, c) => c.x }._1

  def main(args: Array[String]): Unit =
    val limit: Int = 1000
    val result: Int = findMaximalFundamentalSolution(limit)
    println(result)
}
