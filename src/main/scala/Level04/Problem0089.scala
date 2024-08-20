package Level04

import scala.annotation.tailrec

object Problem0089 {
  private val SimplificationRules: List[(String, String)] =
    List(
      ("DD", "M"),
      ("DCCCC", "CM"),
      ("CCCC", "CD"),
      ("LXXXX", "XC"),
      ("LL", "C"),
      ("XXXX", "XL"),
      ("VV", "X"),
      ("VIIII", "IX"),
      ("IIII", "IV")
    )

  @tailrec
  private def simplify(number: String): String =
    SimplificationRules.find{ case (original, _) => number.containsSlice(original) } match
      case None => number
      case Some((original, replacement)) => simplify(number.replaceFirst(original, replacement))

  def calcSavedCharacters(numerals: Iterator[String]): Int =
    numerals.foldLeft(0){ (acc, numeral) =>
      val simplified: String = simplify(numeral)
      acc + (numeral.length - simplified.length)
    }

  def main(args: Array[String]): Unit =
    val lines: Iterator[String] = scala.io.Source.fromResource("input0089.txt").getLines()
    val result: Int = calcSavedCharacters(lines)
    println(result)
}
