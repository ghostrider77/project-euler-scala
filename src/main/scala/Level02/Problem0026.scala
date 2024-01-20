package Level02

import scala.annotation.tailrec

object Problem0026 {
  private def decimalRecurringCycleLength(n: Int): Int =
    @tailrec
    def loop(remainders: List[Int], m: Int): Int =
      val r: Int = m % n
      remainders.indexOf(r) match
        case -1 => loop(r :: remainders, 10*r)
        case ix => ix + 1

    loop(Nil, 1)

  def calcNumberWithLongestRecurringCycle(limit: Int): Int =
    (1 until limit)
      .iterator
      .map(decimalRecurringCycleLength)
      .zipWithIndex
      .maxBy{ case (cycleLength, _) => cycleLength }
      ._2 + 1

  def main(args: Array[String]): Unit =
    val limit: Int = 1000
    val result: Int = calcNumberWithLongestRecurringCycle(limit)
    println(result)
}
