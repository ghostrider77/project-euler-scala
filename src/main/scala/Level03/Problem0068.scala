package Level03

object Problem0068 {
  private val lineIndices: List[List[Int]] =
    List(List(0, 1, 2), List(3, 2, 4), List(5, 4, 6), List(7, 6, 8), List(9, 8, 1))

  private def getStringRepresentation(p: Vector[Int]): String =
    val size: Int = lineIndices.length
    val doubleList: List[List[Int]] = lineIndices ::: lineIndices
    val cyclicalIndices: List[List[List[Int]]] = (0 until size).map(k => doubleList.slice(k, k + size)).toList
    val intlists: List[List[Int]] = cyclicalIndices.map(indices => indices.flatMap(_.map(p(_))))
    intlists.minBy(_.head).mkString

  private def calcMaximalDigitString(numbers: Vector[Int]): String =
    def isValid(perm: Vector[Int]): Boolean =
      val lineSum: Int = perm.take(3).sum
      lineIndices.forall(indices => indices.map(perm(_)).sum == lineSum)

    numbers
      .permutations
      .filter(isValid)
      .map(getStringRepresentation)
      .filter(_.length == 16)
      .max

  def main(args: Array[String]): Unit =
    val numbers: Vector[Int] = (1 to 10).toVector
    val result: String = calcMaximalDigitString(numbers)
    println(result)
}
