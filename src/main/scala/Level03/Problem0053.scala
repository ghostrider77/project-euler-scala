package Level03

object Problem0053 {
  def calcNumberOfLargeBinomialCoefficient(nMax: Int, limit: Int): Int =
    val pascalTriangle: Array[Array[Int]] = Array.fill(nMax + 1, nMax + 1)(1)
    for {
      row <- 1 to nMax
      col <- 1 until row
    } pascalTriangle(row)(col) = math.min(pascalTriangle(row - 1)(col - 1) + pascalTriangle(row - 1)(col), limit)

    pascalTriangle.foldLeft(0)((acc, row) => acc + row.count(_ == limit))

  def main(args: Array[String]): Unit =
    val limit: Int = 1000000
    val n: Int = 100
    val result: Int = calcNumberOfLargeBinomialCoefficient(n, limit)
    println(result)
}
