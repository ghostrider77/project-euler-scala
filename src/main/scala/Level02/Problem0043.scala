package Level02

object Problem0043 {
  private val Divisors: List[Int] = List(2, 3, 5, 7, 11, 13, 17)

  private def sumOfSubstringPandigitalNumbers(): Long =
    def isDivisible(digits: List[Int]): Boolean =
      digits.sliding(3).drop(1).zip(Divisors).forall{ case (ds, d) => ds.mkString.toInt % d == 0 }

    val pandigitals: List[List[Int]] =
      (0 to 9).permutations.map(_.toList).collect{ case number @ h :: _ if h != 0 => number }.toList
    pandigitals.foldLeft(0L)((acc, n) => if isDivisible(n) then acc + n.mkString.toLong else acc)

  def main(args: Array[String]): Unit =
    val result: Long = sumOfSubstringPandigitalNumbers()
    println(result)
}
