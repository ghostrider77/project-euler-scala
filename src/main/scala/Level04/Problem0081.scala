package Level04

object Problem0081 {
  private def parseMatrix(lines: Iterator[String]): Vector[Vector[Int]] =
    lines.map(_.split(",").map(_.toInt).toVector).toVector

  def calcShortestPath(matrix: Vector[Vector[Int]]): Int =
    val nrRows: Int = matrix.length
    val nrCols: Int = matrix.head.length

    val minPathSum: Array[Array[Int]] = Array.fill(nrRows, nrCols)(0)
    minPathSum(0)(0) = matrix(0)(0)
    (1 until nrRows).foreach(i => minPathSum(i)(0) = minPathSum(i - 1)(0) + matrix(i)(0))
    (1 until nrCols).foreach(j => minPathSum(0)(j) = minPathSum(0)(j - 1) + matrix(0)(j))
    for {
      i <- 1 until nrRows
      j <- 1 until nrCols
    } minPathSum(i)(j) = math.min(minPathSum(i)(j - 1), minPathSum(i - 1)(j)) + matrix(i)(j)
    minPathSum(nrRows - 1)(nrCols - 1)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0081.txt").getLines()
    val matrix: Vector[Vector[Int]] = parseMatrix(lines)
    val result: Int = calcShortestPath(matrix)
    println(result)
}
