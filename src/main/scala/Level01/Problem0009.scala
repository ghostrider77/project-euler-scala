package Level01

object Problem0009 {
  def calcPythagoreanTripletProduct(s: Int): Int =
    val triplets: Seq[(Int, Int, Int)] =
      for {
        c <- 3 until s
        b <- 2 until c
        a = s - (b + c)
        if a > 0 && (c * c == a * a + b * b)
      } yield (a, b, c)
    triplets.headOption match {
      case None => throw Exception("No Pythagorean triplets found.")
      case Some((a, b, c)) => a * b * c
    }

  def main(args: Array[String]): Unit =
    val s: Int = 1000
    val result: Int = calcPythagoreanTripletProduct(s)
    println(result)
}
