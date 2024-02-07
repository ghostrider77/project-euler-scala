package Level02

import scala.annotation.tailrec

object Problem0044 {
  private def isPentagonal(k: Long): Boolean =
    val n: Double = (1 + math.sqrt(1.0 + 24*k)) / 6
    n.toInt == n

  private def calcSmallestPentagonalDifference(): Long =
    @tailrec
    def loop(pentagonNumbers: List[Long], n: Long): Long =
      val p: Long = n * (3*n - 1) / 2
      pentagonNumbers match
        case Nil => loop(p :: pentagonNumbers, n + 1)
        case _ =>
          val smallestDiff: Option[Long] =
            pentagonNumbers
              .flatMap(n => if isPentagonal(p + n) && pentagonNumbers.contains(p - n) then Some(p - n) else None)
              .headOption
          smallestDiff match
            case None => loop(p :: pentagonNumbers, n + 1)
            case Some(difference) => difference

    loop(Nil, 1L)

  def main(args: Array[String]): Unit =
    val result: Long = calcSmallestPentagonalDifference()
    println(result)
}
