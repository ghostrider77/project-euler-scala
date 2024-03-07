package Level03

object Problem0072 {
  def nrProperReducedFractions(denominatorLimit: Int): Long =
    val phiValues: Array[Int] = Array.tabulate(denominatorLimit + 1)(identity)
    (2 to denominatorLimit).foreach{ ix =>
      val x: Int = phiValues(ix)
      if x == ix then
        (x to denominatorLimit by x).foreach{ jy =>
          val y = phiValues(jy)
          phiValues(jy) = y - y / x
        }
    }

    phiValues.drop(2).foldLeft(0L)(_ + _)

  def main(args: Array[String]): Unit =
    val denominatorBound: Int = 1000000
    val result: Long = nrProperReducedFractions(denominatorBound)
    println(result)
}
