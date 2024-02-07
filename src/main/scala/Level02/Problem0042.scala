package Level02

object Problem0042 {
  private val Alphabet: List[Char] = ('A' to 'Z').toList

  def calcNumberOfTriangleWords(words: List[String]): Int =
    def calcWordValue(word: String): Int = word.foldLeft(0)((acc, char) => acc + Alphabet.indexOf(char) + 1)
    val wordValues: List[Int] = words.map(calcWordValue)
    wordValues.maxOption match
      case None => 0
      case Some(maxValue) =>
        val triangleNumbers: Set[Int] = Iterator.from(1).map(n => n * (n + 1) / 2).takeWhile(_ <= maxValue).toSet
        wordValues.count(triangleNumbers.contains)

  def main(args: Array[String]): Unit =
    val line: String = scala.io.Source.fromResource("input0042.txt").getLines().next()
    val words: List[String] = line.split(",").toList
    val result: Int = calcNumberOfTriangleWords(words)
    println(result)
}
