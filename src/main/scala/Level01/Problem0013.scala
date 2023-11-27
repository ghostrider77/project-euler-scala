package Level01

object Problem0013 {
  def calcLargeSum(xs: List[BigInt], prefixSize: Int): String =
    xs.sum.toString.take(prefixSize)

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0013.txt").getLines()
    val prefixSize: Int = 10
    val numbers: List[BigInt] = lines.map(BigInt(_)).toList
    val result: String = calcLargeSum(numbers, prefixSize)
    println(result)
}
