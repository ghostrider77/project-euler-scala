package Level03

object Problem0067 {
  private def readTriangleGrid(lines: Iterator[String]): Vector[Vector[Int]] =
    lines.map(row => row.split(" ").map(_.toInt).toVector).toVector

  def calcMaxPathSum(grid: Vector[Vector[Int]]): Int =
    val n: Int = grid.length
    val maxPathSum: Array[Array[Int]] = Array.fill(n, n)(0)
    maxPathSum(0)(0) = grid(0)(0)
    for {
      ix <- 1 until n
      jy <- 0 to ix
    } {
      if jy == 0 then maxPathSum(ix)(jy) = grid(ix)(jy) + maxPathSum(ix - 1)(jy)
      else maxPathSum(ix)(jy) = grid(ix)(jy) + math.max(maxPathSum(ix - 1)(jy), maxPathSum(ix - 1)(jy - 1))
    }
    maxPathSum(n - 1).max

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0067.txt").getLines()
    val grid: Vector[Vector[Int]] = readTriangleGrid(lines)
    val result: Int = calcMaxPathSum(grid)
    println(result)
}
