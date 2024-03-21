package Level03

import scala.annotation.tailrec

object Problem0075 {
  @tailrec
  private def calcGcd(a: Int, b: Int): Int =
    if b == 0 then a
    else calcGcd(b, a % b)

  def singularIntegerRightTriangle(limit: Int): Int =
    val upperLimit: Int = math.sqrt(limit / 2.0).toInt
    val tripletPerimeters: Iterator[Int] =
      (for {
        n <- (1 to upperLimit).iterator
        m <- (n + 1) to upperLimit
        if (m - n) % 2 != 0 && calcGcd(m, n) == 1
        a: Int = m * m - n * n
        b: Int = 2 * m * n
        c: Int = m * m + n * n
        perimeter: Int = a + b + c
      } yield Iterator.from(1).map(_ * perimeter).takeWhile(_ <= limit)).flatten
    val occurrences: Map[Int, Int] = tripletPerimeters.toList.groupMapReduce(identity)(_ => 1)(_ + _)
    occurrences.valuesIterator.count(_ == 1)

  def main(args: Array[String]): Unit =
    val limit: Int = 1500000
    val result: Int = singularIntegerRightTriangle(limit)
    println(result)
}
