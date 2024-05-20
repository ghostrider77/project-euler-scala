package Level04

import scala.annotation.tailrec

object Problem0100 {
  def solveNegativePellEquation(limit: Long): Long =
    @tailrec
    def loop(x: Long, y: Long): Long =
      if (x + 1) / 2 < limit then loop(3*x + 4*y, 2*x + 3*y)
      else (y + 1) / 2

    loop(1L, 1L)

  def main(args: Array[String]): Unit =
    val limit: Long = 1000000000000L
    val result: Long = solveNegativePellEquation(limit)
    println(result)
}
