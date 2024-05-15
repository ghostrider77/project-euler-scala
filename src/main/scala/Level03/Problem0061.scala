package Level03

import scala.annotation.tailrec

object Problem0061 {
  private def calcFigurateNumbers(lower: Int, upper: Int): List[List[String]] =
    def getFigurates(func: Int => Int): Iterator[Int] =
      Iterator.from(1).map(func).dropWhile(_ < lower).takeWhile(_ < upper)

    val triangles: Iterator[Int] = getFigurates(n => n*(n + 1) / 2)
    val squares: Iterator[Int] = getFigurates(n => n*n)
    val pentagonals: Iterator[Int] = getFigurates(n => n*(3*n - 1) / 2)
    val hexagonals: Iterator[Int] = getFigurates(n => n*(2*n - 1))
    val heptagonals: Iterator[Int] = getFigurates(n => n*(5*n - 3) / 2)
    val octagonals: Iterator[Int] = getFigurates(n => n*(3*n - 2))
    val figurateNumbers: List[Iterator[Int]] =
      List(triangles, squares, pentagonals, hexagonals, heptagonals, octagonals)
    figurateNumbers.map(_.map(_.toString).toList)

  private def generateCandidateSolutions(figurateNumbers: List[List[String]]): List[List[String]] =
    @tailrec
    def loop(acc: List[List[String]], figs: List[List[String]]): List[List[String]] = figs match
      case Nil => acc.filter(xs => xs.head.takeRight(2) == xs.last.take(2))
      case fs :: rest =>
        val candiates: List[List[String]] = fs.flatMap(f => acc.map(as => f :: as))
        val validCandidates: List[List[String]] =
          candiates.collect { case candidate @ x :: y :: _ if x.take(2) == y.takeRight(2) => candidate }
        loop(validCandidates, rest)

    loop(figurateNumbers.head.map(List(_)), figurateNumbers.tail)

  private def findCyclicalFigurateNumbers(lower: Int, upper: Int): Int =
    val figurateNumbers: List[List[String]] = calcFigurateNumbers(lower, upper)
    val allPossibleFigurateOrders: List[List[List[String]]] = figurateNumbers.permutations.toList
    allPossibleFigurateOrders.flatMap(generateCandidateSolutions).headOption match
      case Some(solution) => solution.map(_.toInt).sum
      case None => throw Exception("No solution was found.")

  def main(args: Array[String]): Unit =
    val lower: Int = 1000
    val upper: Int = 10000
    val result: Int = findCyclicalFigurateNumbers(lower, upper)
    println(result)
}
