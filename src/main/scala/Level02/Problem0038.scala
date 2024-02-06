package Level02

import scala.annotation.tailrec

object Problem0038 {
  private val Digits: List[Int] = (1 to 9).toList

  private def isPandigital(n: Int): Boolean =
    n.toString.map(_.asDigit).sorted == Digits

  private def generatePandigitalProduct(n: Int): Option[Int] =
    @tailrec
    def loop(acc: String, k: Int): Option[Int] =
      val length: Int = acc.length
      if length > 9 then None
      else if length == 9 then Some(acc.toInt).filter(isPandigital)
      else loop(s"$acc${k*n}", k + 1)

    loop(n.toString, 2)

  private def calcLargestPandigitalProduct(): Int =
    (1 to 9876).flatMap(generatePandigitalProduct).maxOption match
      case None => throw Exception("There exists a solution to this problem.")
      case Some(n) => n

  def main(args: Array[String]): Unit =
    val result: Int = calcLargestPandigitalProduct()
    println(result)
}
