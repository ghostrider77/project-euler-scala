package Level03

object Problem0070 {
  private def calcTotientValues(limit: Int): Vector[Int] =
    val phiValues: Array[Int] = Array.tabulate(limit + 1)(identity)
    (2 to limit).foreach { ix =>
      val x: Int = phiValues(ix)
      if x == ix then
        (x to limit by x).foreach { jy =>
          val y = phiValues(jy)
          phiValues(jy) = y - y / x
        }
    }
    phiValues.toVector

  private def calcMinimumTotientQuotient(limit: Int): Int =
    def isPermutation(n: Int, m: Int): Boolean =
      n.toString.sorted == m.toString.sorted

    val phiValues: Vector[Int] = calcTotientValues(limit)
    val (nMin, q): (Int, Double) = (3 to limit by 2).foldLeft((1, 2.0)){
      case (acc @ (_, minRatio), n) =>
        val phi: Int = phiValues(n)
        val r: Double = n / phi.toDouble
        if r < minRatio && isPermutation(n, phi) then (n, r) else acc
    }
    nMin

  def main(args: Array[String]): Unit =
    val limit: Int = 10000000
    val result: Int = calcMinimumTotientQuotient(limit)
    println(result)
}
