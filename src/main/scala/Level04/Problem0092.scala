package Level04

import scala.annotation.tailrec

object Problem0092 {
  private def numberEndsAt89(n: Int): Boolean =
    def squareOfDigits(k: Int): Int =
      k.toString.foldLeft(0){(acc, char) =>
        val d: Int = char.asDigit
        d * d + acc
      }

    @tailrec
    def loop(k: Int): Boolean =
      if k == 89 then true
      else if k == 1 then false
      else loop(squareOfDigits(k))

    loop(n)

  private def solveProblem(limit: Int): Int =
    (1 to limit).count(numberEndsAt89)

  def main(args: Array[String]): Unit =
    val limit: Int = 10000000
    val result: Int = solveProblem(limit)
    println(result)
}
