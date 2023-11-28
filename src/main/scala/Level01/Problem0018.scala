package Level01

object Problem0018 {
  private def readTriangleGrid(lines: Iterator[String]): Vector[Vector[Int]] =
    lines.map(row => row.split(" ").map(_.toInt).toVector).toVector

  def maxPathSum(grid: Vector[Vector[Int]]): Int =
    val height: Int = grid.length

    def generateAllPaths(i: Int, j: Int): List[List[Int]] =
      if i == height - 1 then List(List(grid(i)(j)))
      else
        val pathsDown: List[List[Int]] = generateAllPaths(i + 1, j)
        val pathsRight: List[List[Int]] = generateAllPaths(i + 1, j + 1)
        (pathsDown ::: pathsRight).map(grid(i)(j) :: _)

    val paths: List[List[Int]] = generateAllPaths(0, 0)
    paths.foldLeft(Int.MinValue)((acc, path) => acc max path.sum)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0018.txt").getLines()
    val grid: Vector[Vector[Int]] = readTriangleGrid(lines)
    val result: Int = maxPathSum(grid)
    println(result)
}
