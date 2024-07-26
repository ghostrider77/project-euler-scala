package Level04

import scala.annotation.tailrec

object Problem0086 {
  private def nrIntegerShortestPaths(a: Int): Int =
    def isPythagorean(x: Int, y: Int): Boolean =
      val z: Double = math.hypot(x, y)
      math.abs(z.toInt - z) < 1e-8

    (1 to 2*a).foldLeft(0){
      (acc, bc) => if isPythagorean(a, bc) then acc + (math.min(a + 1, bc) - (bc + 1) / 2) else acc
    }

  def smallestDimensionForCuboidRoutes(limit: Int): Int =
    @tailrec
    def loop(acc: Int, a: Int): Int =
      if acc >= limit then a - 1
      else loop(acc + nrIntegerShortestPaths(a), a + 1)

    loop(0, 1)

  def main(args: Array[String]): Unit =
    val limit: Int = 1000000
    val result: Int = smallestDimensionForCuboidRoutes(limit)
    println(result)
}
