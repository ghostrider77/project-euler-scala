package Level02

object Problem0029 {
  def distinctPowers(n: Int): Int =
    (for {
      a <- (2 to n).toSet
      b <- 2 to n
    } yield BigInt(a).pow(b)).size

  def main(args: Array[String]): Unit =
    val limit: Int = 100
    val result: Int = distinctPowers(limit)
    println(result)
}
