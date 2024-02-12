package Level03

object Problem0056 {
  def calcMaximalDigitSum(limit: Int): Int =
    val powers: Iterator[BigInt] =
      for {
        a <- (1 until limit).iterator
        b <- 1 until limit
      } yield BigInt(a).pow(b)
    powers.map(_.toString.map(_.asDigit).sum).max

  def main(args: Array[String]): Unit =
    val limit: Int = 100
    val result: Int = calcMaximalDigitSum(limit)
    println(result)
}
