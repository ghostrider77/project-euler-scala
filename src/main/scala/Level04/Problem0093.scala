package Level04

object Problem0093 {
  private val AllDigits: List[Int] = (1 to 9).toList
  private val Operators: List[(Double, Double) => Double] = List(_ + _, _ - _, _ * _, _ / _)

  private def isInteger(x: Double): Boolean =
    math.abs(x - math.round(x)) <= 1e-8

  private def performAllOperations(digits: List[Int]): Set[Int] =
    val permutations: List[(Int, Int, Int, Int)] =
      digits.permutations.collect{ case List(a, b, c, d) => (a, b, c, d) }.toList
    val calculatedValues: List[List[Double]] = for {
      (a, b, c, d) <- permutations
      op1 <- Operators
      op2 <- Operators
      op3 <- Operators
    } yield List(
      op2(op1(a, b), op3(c, d)),
      op3(op2(op1(a, b), c), d),
      op1(a, op2(b, op3(c, d))),
      op3(op1(a, op2(b, c)), d),
      op1(a, op3(op2(b, c), d))
    )
    calculatedValues.flatten.collect{ case v if v.isFinite && v > 0 && isInteger(v) => v.toInt }.toSet

  private def calcLongestConsecutiveLength(digits: List[Int]): Int =
    val possibleValues: Set[Int] = performAllOperations(digits)
    (1 to possibleValues.size).indexWhere(k => !possibleValues.contains(k))

  private def findMostExpressiveDigits(): List[Int] =
    AllDigits.combinations(4).maxBy(calcLongestConsecutiveLength)

  def main(args: Array[String]): Unit =
    val result: List[Int] = findMostExpressiveDigits()
    println(result.mkString)
}
