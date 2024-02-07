package Level02

object Problem0032 {
  private val Digits: List[Int] = (1 to 9).toList

  private def isPandigital(n: Int): Boolean =
    n.toString.map(_.asDigit).sorted == Digits

  private def sumOfPandigitalProducts(): Int =
    (for {
      a <- (1 to 99).toSet
      b <- 100 to 9999
      product: Int = a * b
      concatenated: String = s"$a$b$product"
      if concatenated.length == 9 && isPandigital(concatenated.toInt)
    } yield product).sum

  def main(args: Array[String]): Unit =
    val result: Int = sumOfPandigitalProducts()
    println(result)
}
