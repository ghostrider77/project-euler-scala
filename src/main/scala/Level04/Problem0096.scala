package Level04

import scala.annotation.tailrec
import scala.language.implicitConversions

object Problem0096 {
  private val Size: Int = 9
  private val SquareSize: Int = 3

  case class Coord(x: Int, y: Int)

  case class Puzzle(board: Map[Coord, Int]) {
    override def toString: String =
      (0 until Size).map(x => (0 until Size).map(y => board(Coord(x, y))).mkString(" ")).mkString("\n")

    def cornerValue: Int = s"${board(Coord(0, 0))}${board(Coord(0, 1))}${board(Coord(0, 2))}".toInt

    def update(coord: Coord, value: Int): Puzzle =
      board.updated(coord, value)

    def emptyCells(): List[(Coord, List[Int])] =
      (for {
        x <- 0 until Size
        y <- 0 until Size
        c = Coord(x, y)
        if board(c) == 0
      } yield (c, getPossibleValues(c))).toList.sortBy{ case (_, values) => values.length }

    private def getPossibleValues(coord: Coord): List[Int] =
      board(coord) match
        case 0 =>
          val row: Set[Int] = (0 until Size).iterator.map(y => board(Coord(coord.x, y))).toSet
          val col: Set[Int] = (0 until Size).iterator.map(x => board(Coord(x, coord.y))).toSet
          val square: Set[Int] = getValuesInSquare(coord)
          (1 to Size).toSet.diff(row.union(col).union(square)).toList
        case _ => Nil

    private def getValuesInSquare(coord: Coord): Set[Int] =
      val x: Int = coord.x / SquareSize
      val y: Int = coord.y / SquareSize
      (for {
        i <- SquareSize*x until SquareSize*x + SquareSize
        j <- SquareSize*y until SquareSize*y + SquareSize
      } yield board(Coord(i, j))).toSet
  }

  given Conversion[Map[Coord, Int], Puzzle] = (board: Map[Coord, Int]) => Puzzle(board)

  def readPuzzle(lines: List[String]): Puzzle =
    lines
      .zipWithIndex
      .flatMap{ case (line, x) => line.zipWithIndex.map{ case (v, y) => (Coord(x, y), v.asDigit) } }
      .toMap

  private def readBoards(lines: Iterator[String]): List[Puzzle] =
    @tailrec
    def loop(acc: List[Puzzle]): List[Puzzle] =
      if lines.isEmpty then acc.reverse
      else
        val puzzle: Puzzle = readPuzzle(lines.slice(1, Size + 1).toList)
        loop(puzzle :: acc)

    loop(Nil)

  private def solvePuzzle(puzzle: Puzzle): List[Puzzle] =
    puzzle.emptyCells() match
      case Nil => List(puzzle)
      case (_, Nil) :: _ => Nil
      case (coord, possibleValues) :: _ => possibleValues.flatMap(value => solvePuzzle(puzzle.update(coord, value)))

  def solveSudokuPuzzles(puzzles: List[Puzzle]): Int =
    @tailrec
    def loop(acc: Int, ps: List[Puzzle]): Int = ps match
      case Nil => acc
      case p :: pss => solvePuzzle(p) match
        case List(result) => loop(acc + result.cornerValue, pss)
        case _ => throw Exception("No unique solution was found.")

    loop(0, puzzles)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0096.txt").getLines()
    val puzzles: List[Puzzle] = readBoards(lines)
    val result: Int = solveSudokuPuzzles(puzzles)
    println(result)
}
