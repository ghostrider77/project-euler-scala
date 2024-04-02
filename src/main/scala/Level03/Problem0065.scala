package Level03

import scala.annotation.tailrec

object Problem0065 {
  private case class Convergent(n: BigInt, d: BigInt)

  def sumOfConvergentNumeratorDigits(limit: Int): Int =
    val a: Int = 1

    @tailrec
    def loop(c0: Convergent, c1: Convergent, k: Int): Convergent =
      if k == limit then c0
      else
        val Convergent(n0, d0) = c0
        val Convergent(n1, d1) = c1
        val b: Int = if k % 3 == 1 then 2*(k / 3 + 1) else 1
        loop(c1, Convergent(b*n1 + a*n0, b*d1 + a*d0), k + 1)

    val Convergent(n, _) = loop(Convergent(2, 1), Convergent(3, 1), 1)
    n.toString.foldLeft(0)((acc, d) => acc + d.asDigit)

  def main(args: Array[String]): Unit =
    val limit: Int = 100
    val result: Int = sumOfConvergentNumeratorDigits(limit)
    println(result)
}
