package Level02

object Problem0031 {
  def coinSums(amount: Int, coins: List[Int]): Int =
    if amount == 0 then 1
    else coins match
      case Nil => 0
      case coin :: rest => (amount to 0 by -coin).foldLeft(0)((acc, m) => acc + coinSums(m, rest))

  def main(args: Array[String]): Unit =
    val amount: Int = 200
    val coins: List[Int] = List(200, 100, 50, 20, 10, 5, 2, 1)
    val result: Int = coinSums(amount, coins)
    println(result)
}
