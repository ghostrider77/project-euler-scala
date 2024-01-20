package Level01

object Problem0017 {
  private val Numbers: List[String] = List(
    "one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten",
    "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"
  )

  private val Tens: List[String] = List("twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")

  private def calcNumberLetterSum(): Int =
    val one: Int = Numbers.head.length
    val numberSum: Int = Numbers.map(_.length).sum
    val sumFrom1To9: Int = Numbers.take(9).map(_.length).sum
    val sumUpTo99: Int = Tens.foldLeft(numberSum)((acc, n) => acc + 10*n.length + sumFrom1To9)
    val hundred = "hundred".length
    val thousand = "thousand".length
    Numbers.take(9).foldLeft(sumUpTo99 + one + thousand){
      (acc, n) =>
        val l: Int = n.length
        acc + l + hundred + 99*(l + hundred + one) + sumUpTo99
    }

  def main(args: Array[String]): Unit =
    val result: Int = calcNumberLetterSum()
    println(result)
}
