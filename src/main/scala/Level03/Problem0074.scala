package Level03

import scala.annotation.tailrec

object Problem0074 {
  private val Factorials: Vector[Int] = (1 to 9).scanLeft(1)(_ * _).toVector

  def calcFactorialChainLength(n: Int): Int =
    @tailrec
    def loop(chain: Set[Int], k: Int): Int =
      if chain.contains(k) then chain.size
      else
        val digitFactorialSum: Int = k.toString.foldLeft(0)((acc, d) => acc + Factorials(d.asDigit))
        loop(chain + k, digitFactorialSum)

    loop(Set(), n)

  private def calcNrFactorialChains(size: Int, limit: Int): Int =
    (1 until limit).count(calcFactorialChainLength(_) == size)

  def main(args: Array[String]): Unit =
    val size: Int = 60
    val limit: Int = 1000000
    val result: Int = calcNrFactorialChains(size, limit)
    println(result)
}
