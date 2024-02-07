package Level02

import scala.annotation.tailrec

object Problem0045 {
  def findHexagonalPentagonalNumber(nHex: Long, nPent: Long): Long =
    def calcHexagonal(n: Long): Long = n * (2*n - 1)
    def calcPentagonal(n: Long): Long = n * (3*n - 1) / 2

    @tailrec
    def loop(hexagonals: Set[Long], nH: Long, nP: Long): Long =
      val mH: Long = nH + 1
      val mP: Long = nP + 1
      val pentagonal: Long = calcPentagonal(mP)
      val hexagonal: Long = calcHexagonal(mH)
      if hexagonals.contains(pentagonal) then pentagonal
      else loop(hexagonals + hexagonal, mH, mP)

    loop(Set(calcHexagonal(nHex)), nHex, nPent)

  def main(args: Array[String]): Unit =
    val nH: Long = 143
    val nP: Long = 165
    val result: Long = findHexagonalPentagonalNumber(nH, nP)
    println(result)
}
