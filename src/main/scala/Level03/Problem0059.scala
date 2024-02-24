package Level03

object Problem0059 {
  private val MostCommonEnglishWords: Set[String] = Set("the", "be", "to", "of", "and", "a", "in", "that", "have", "i")

  private def decodeText(encryptedText: List[Int], key: List[Int]): List[Int] =
    val repeatedKey: Iterator[Int] = Iterator.continually(key).flatten
    encryptedText.iterator.zip(repeatedKey).map(_ ^ _).toList

  private def calcAsciiCodeSumOfDecryptedText(encodedText: List[Int]): Int =
    val keys: List[List[Int]] =
      val lowerCaseAsciiCodes: List[Int] = ('a' to 'z').map(_.toInt).toList
      for {
        a <- lowerCaseAsciiCodes
        b <- lowerCaseAsciiCodes
        c <- lowerCaseAsciiCodes
      } yield List(a, b, c)
    val (maxSumOfAsciiValues, _): (Int, Int) = keys.foldLeft((0, 0)){
      case (acc @ (_, maxEnglishWordCount), key) =>
        val decodedText: List[Int] = decodeText(encodedText, key)
        val recoveredText: List[String] = decodedText.map(_.toChar).mkString.split(" ").toList
        val wordCount: Int = recoveredText.count(MostCommonEnglishWords.contains)
        if wordCount > maxEnglishWordCount then (decodedText.sum, wordCount) else acc
    }
    maxSumOfAsciiValues

  def main(args: Array[String]): Unit =
    val line: String = scala.io.Source.fromResource("input0059.txt").getLines().next()
    val encodedText: List[Int] = line.split(",").map(_.toInt).toList
    val result: Int = calcAsciiCodeSumOfDecryptedText(encodedText)
    println(result)
}
