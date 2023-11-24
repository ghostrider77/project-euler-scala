package Level01

object Problem0008 {
  def calcLargestProduct(numbers: List[Int], windowSize: Int): Long =
    numbers.sliding(windowSize).map(_.map(_.toLong).product).max

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0008.txt").getLines()
    val number: List[Int] = lines.flatMap(_.toCharArray.map(_.asDigit)).toList
    val result: Long = calcLargestProduct(number, 13)
    println(result)
}
