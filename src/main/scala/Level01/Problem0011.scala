package Level01

object Problem0011 {
  case class Grid(cells: Vector[Vector[Int]], nrRows: Int, nrCols: Int):
    def contains(x: Int, y: Int): Boolean =
      0 <= x && x < nrCols && 0 <= y && y < nrRows

  private def parseGrid(lines: Iterator[String]): Grid =
    val cells: Vector[Vector[Int]] = lines.map(row => row.split(" ").map(_.toInt).toVector).toVector
    Grid(cells, cells.length, cells.head.length)

  def calcMaximalGridProduct(grid: Grid, windowSize: Int): Int =
    val startCells: Iterator[(Int, Int)] =
      for {
        x <- (0 until grid.nrCols).iterator
        y <- 0 until grid.nrRows
      } yield (x, y)
    val adjacentCells: Iterator[List[(Int, Int)]] =
      startCells.flatMap { case (x, y) =>
        List(
          (0 until windowSize).map(k => (x + k, y)).toList,
          (0 until windowSize).map(k => (x, y + k)).toList,
          (0 until windowSize).map(k => (x + k, y + k)).toList,
          (0 until windowSize).map(k => (x - k, y + k)).toList
        )
      }
    adjacentCells
      .filter(_.forall(grid.contains.tupled))
      .map{ coords => coords.map{ case (x, y) => grid.cells(x)(y)}.product }
      .max

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0011.txt").getLines()
    val windowSize: Int = 4
    val grid: Grid = parseGrid(lines)
    val result: Int = calcMaximalGridProduct(grid, windowSize)
    println(result)
}
