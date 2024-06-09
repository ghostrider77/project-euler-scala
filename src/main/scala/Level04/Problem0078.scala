package Level04

import scala.annotation.tailrec

object Problem0078 {
  private def calcPartitionNumber(cache: Map[Int, Int], n: Int, modulus: Int): Int =
    @tailrec
    def loop(acc: Int, k: Int): Int =
      val arg1: Int = n - k*(3*k - 1) / 2
      val arg2: Int = n - k*(3*k + 1) / 2
      val sign: Int = if k % 2 == 0 then -1 else 1
      val p1: Int = sign * cache.getOrElse(arg1, 0)
      val p2: Int = sign * cache.getOrElse(arg2, 0)
      val nextAcc: Int = (acc + p1 + p2) % modulus
      if arg2 < 0 then nextAcc else loop(nextAcc, k + 1)

    loop(0, 1)

  def calcSmallestValue(modulus: Int): Int =
    @tailrec
    def loop(acc: Map[Int, Int], n: Int): Int =
      val pn: Int = calcPartitionNumber(acc, n, modulus)
      if pn == 0 then n else loop(acc.updated(n, pn), n + 1)

    loop(Map(0 -> 1), 1)

  def main(args: Array[String]): Unit =
    val modulus: Int = 1000000
    val result: Int = calcSmallestValue(modulus)
    println(result)
}
