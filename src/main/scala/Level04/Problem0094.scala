package Level04

import scala.annotation.tailrec

object Problem0094 {
  def calcPerimeterSum(limit: Int): Long =
    @tailrec
    def loop(acc: Long, x: Int, y: Int): Long =
      val side1: Int = (2*x + 1) / 3
      val side2: Int = (2*x - 1) / 3
      val perimeter1: Int = 3*side1 + 1
      val perimeter2: Int = 3*side2 - 1
      if perimeter1 > limit && perimeter2 > limit then acc
      else
        val acc1: Long = if (2*x + 1) % 3 == 0 && perimeter1 <= limit then acc + perimeter1 else acc
        val acc2: Long = if (2*x - 1) % 3 == 0 && perimeter2 <= limit then acc1 + perimeter2 else acc1
        loop(acc2, 2*x + 3*y, 2*y + x)

    loop(0, 7, 4)

  def main(args: Array[String]): Unit =
    val limit: Int = 1000000000
    val result: Long = calcPerimeterSum(limit)
    println(result)
}
