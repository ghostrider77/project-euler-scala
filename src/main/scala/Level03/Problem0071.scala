package Level03

object Problem0071 {
  private case class Fraction(nominator: Int, denominator: Int)

  def orderedFractions(limit: Int): Int =
    val target: Double = 3 / 7.0
    val (_, Fraction(bestNominator, _)): (Double, Fraction) = (1 to limit).foldLeft((target, Fraction(0, 1))){
      case (acc @ (minDist, _), d) =>
        val n: Int = if d % 7 == 0 then 3 * d / 7 - 1 else 3 * d / 7
        val distance = target - n / d.toDouble
        if distance < minDist then (distance, Fraction(n, d)) else acc
    }
    bestNominator

  def main(args: Array[String]): Unit =
    val limit: Int = 1000000
    val result: Int = orderedFractions(limit)
    println(result)
}
