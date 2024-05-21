package Level04

object Problem0099 {
  case class Number(base: Int, exponent: Int)

  private def readNumbers(lines: Iterator[String]): List[Number] =
    def parseLine(line: String): Number =
      line.split(",").map(_.toInt).toList match
        case List(base, exponent) => Number(base, exponent)
        case _ => throw Exception("Malformed input.")

    lines.map(parseLine).toList

  def findIndexOfLargestNumber(numbers: List[Number]): Int =
    numbers
      .zip(Iterator.from(1))
      .maxBy{ case (Number(base, exponent), _) => exponent * math.log(base) }
      ._2

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0099.txt").getLines()
    val numbers: List[Number] = readNumbers(lines)
    val result: Int = findIndexOfLargestNumber(numbers)
    println(result)
}
