package Level03

import scala.annotation.tailrec

object Problem0057 {
  private case class Convergent(n: BigInt, d: BigInt)

  def countConvergentsWithLargeNominators(limit: Int): Int =
    val a: Int = 1
    val b: Int = 2

    @tailrec
    def loop(acc: Int, c0: Convergent, c1: Convergent, k: Int): Int =
      if k == limit then acc
      else
        val c2 = Convergent(b * c1.n + a * c0.n, b * c1.d + a * c0.d)
        if c1.n.toString.length > c1.d.toString.length then loop(acc + 1, c1, c2, k + 1)
        else loop(acc, c1, c2, k + 1)

    loop(0, Convergent(1, 1), Convergent(3, 2), 0)

  def main(args: Array[String]): Unit =
    val limit: Int = 1000
    val result: Int = countConvergentsWithLargeNominators(limit)
    println(result)
}
