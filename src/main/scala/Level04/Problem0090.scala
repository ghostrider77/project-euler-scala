package Level04

object Problem0090 {
  private def areCubesSuitable(cube1: Set[Char], cube2: Set[Char], squares: List[String]): Boolean =
    def doContainSquare(square: String): Boolean = square.toCharArray.toList match
      case List(a, b) =>
        val b1: Boolean = cube1.contains(a) && cube2.contains(b)
        val b2: Boolean = cube1.contains(b) && cube2.contains(a)
        b1 || b2
      case _ => throw Exception(s"Unknown square number $square.")

    squares.forall(doContainSquare)

  private def numberOfDiceArrangements(digits: List[Char]): Int =
    val squares: List[String] = List("01", "04", "09", "16", "25", "36", "49", "64", "81")

    def addMirroredNumber(cube: Set[Char]): Set[Char] =
      if cube.contains('6') then cube + '9'
      else if cube.contains('9') then cube + '6'
      else cube

    val cubeArrangements: List[List[Char]] = digits.combinations(6).toList
    cubeArrangements.zipWithIndex.foldLeft(0){
      case (acc, (cube1, n)) =>
        val c1: Set[Char] = addMirroredNumber(cube1.toSet)
        val count: Int =
          cubeArrangements.drop(n + 1).count(cube2 => areCubesSuitable(c1, addMirroredNumber(cube2.toSet), squares))
        acc + count
    }

  def main(args: Array[String]): Unit =
    val digits: List[Char] = ('0' to '9').toList
    val result: Int = numberOfDiceArrangements(digits)
    println(result)
}
