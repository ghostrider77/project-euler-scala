package Level03

object Problem0063 {
  private def powerfulDigitCount(limit: Int): Int =
    def process(acc: Int, n: Int): Int =
      val nthPowers: List[BigInt] = (1 to 9).map(BigInt(_).pow(n)).toList
      acc + nthPowers.count(_.toString.length == n)

    (1 to limit).foldLeft(0)(process)

  def main(args: Array[String]): Unit =
    val limit: Int = math.ceil(math.log(10.0) / math.log(10.0 / 9)).toInt
    val result: Int = powerfulDigitCount(limit)
    println(result)
}
