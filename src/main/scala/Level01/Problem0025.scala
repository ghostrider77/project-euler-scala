package Level01

object Problem0025 {
  def getIndexOfLargeFibonacciNumber(nrDigits: Int): Int =
    Iterator
      .iterate((BigInt(1), BigInt(1))){ case (a, b) => (b, a + b) }
      .map{ case (a, _) => a.toString.length }
      .zipWithIndex
      .dropWhile{ case (length, _) => length < nrDigits }
      .next()
      ._2 + 1

  def main(args: Array[String]): Unit =
    val nrDigits: Int = 1000
    val result: Int = getIndexOfLargeFibonacciNumber(nrDigits)
    println(result)
}
