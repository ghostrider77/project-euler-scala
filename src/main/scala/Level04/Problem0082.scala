package Level04

import scala.annotation.tailrec

object Problem0082 {
  import scala.collection.mutable.PriorityQueue as Heap

  case class Cell(x: Int, y: Int)
  case class Distance(cell: Cell, distance: Double)

  given distanceOrdering: Ordering[Distance] with
    def compare(d1: Distance, d2: Distance): Int = d2.distance compare d1.distance

  case class Grid(matrix: Vector[Vector[Int]], nrRows: Int, nrCols: Int):
    private def isValidCell(cell: Cell): Boolean =
      0 <= cell.x && cell.x < nrRows && 0 <= cell.y && cell.y < nrCols

    def getNeighbors(cell: Cell): List[Cell] =
      val Cell(x, y) = cell
      val candidates: Iterator[Cell] = Iterator(Cell(x, y + 1), Cell(x - 1, y), Cell(x + 1, y))
      candidates.filter(isValidCell).toList

  private def parseGrid(lines: List[String]): Grid =
    lines.map(_.split(",").map(_.toInt).toVector) match
      case Nil => throw Exception("Empty matrix")
      case matrix @ h :: _ =>
        val nrRows: Int = matrix.length
        val nrCols: Int = h.length
        Grid(matrix.toVector, nrRows, nrCols)

  def calcShortestPath(grid: Grid): Int =
    val Grid(matrix, nrRows, nrCols) = grid
    val heap: Heap[Distance] = Heap()
    (0 until nrRows).foreach{ x => heap.enqueue(Distance(Cell(x, 0), matrix(x)(0))) }

    def isShorter(distances: Map[Cell, Double], dist: Double, v: Cell): Boolean =
      dist + matrix(v.x)(v.y) < distances.getOrElse(v, Double.PositiveInfinity)

    @tailrec
    def loop(distances: Map[Cell, Double], processed: Set[Cell]): Int =
      if heap.isEmpty then (0 until nrRows).foldLeft(Double.PositiveInfinity){
        case (acc, x) => math.min(acc, distances.getOrElse(Cell(x, nrCols - 1), Double.PositiveInfinity))
      }.toInt
      else
        val Distance(cell, d) = heap.dequeue()
        if processed.contains(cell) then loop(distances, processed)
        else
          val updatedDistances: Map[Cell, Double] =
            grid
              .getNeighbors(cell)
              .collect {
                case neighbor if !processed.contains(neighbor) && isShorter(distances, d, neighbor) =>
                  neighbor -> (d + matrix(neighbor.x)(neighbor.y))
              }
              .toMap
          updatedDistances.map{ (neighbor, distance) => Distance(neighbor, distance) }.foreach(heap.enqueue(_))
          loop(distances ++ updatedDistances, processed + cell)

    loop(Map(), Set())

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0082.txt").getLines()
    val grid: Grid = parseGrid(lines.toList)
    val result: Int = calcShortestPath(grid)
    println(result)
}
