package Level04

object Problem0076 {
  def calcNumberOfSums(n: Int): Int =
    val partitionMatrix: Array[Array[Int]] = Array.fill(n - 1, n)(0)
    (0 until n).foreach{ j => partitionMatrix(0)(j) = 1 }
    (0 until n - 1).foreach{ i => partitionMatrix(i)(0) = 1 }

    (1 until n - 1).foreach{ i =>
      (1 until n).foreach { j =>
        if i == j then partitionMatrix(i)(j) = 1 + partitionMatrix(i - 1)(i)
        else if i > j then partitionMatrix(i)(j) = partitionMatrix(j)(j)
        else partitionMatrix(i)(j) = partitionMatrix(i - 1)(j) + partitionMatrix(i)(j - i - 1)
      }
    }
    partitionMatrix(n - 2)(n - 1)

  def main(args: Array[String]): Unit =
    val n: Int = 100
    val result: Int = calcNumberOfSums(n)
    println(result)
}
