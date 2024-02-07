package Level02

object Problem0039 {
  def calcPerimeterWithMostPythagoreanTriples(limit: Int): Int =
    val perimeters: List[Int] =
      for {
        c <- (5 until limit).toList
        b <- 1 until c
        a <- 1 to b
        p = a + b + c
        if a*a + b*b == c*c && p <= limit
      } yield p

    perimeters
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .maxBy{ case (_, occurrence) => occurrence }
      ._1

  def main(args: Array[String]): Unit =
    val limit: Int = 100
    val result: Int = calcPerimeterWithMostPythagoreanTriples(limit)
    println(result)
}
