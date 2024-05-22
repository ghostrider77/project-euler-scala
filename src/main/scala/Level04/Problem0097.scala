package Level04

import scala.annotation.tailrec

object Problem0097 {
  private val Modulus: Long = 10000000000L

  private def moduloPower(base: Int, exponent: Int): Long =
    @tailrec
    def loop(acc: Long, k: Int): Long =
      if k == exponent then acc
      else loop(acc * base % Modulus, k + 1)

    loop(1L, 0)

  def main(args: Array[String]): Unit =
    val exponent: Int = 7830457
    val result: Long = (28433 * moduloPower(2, exponent) + 1) % Modulus
    println("%010d".format(result))
}
