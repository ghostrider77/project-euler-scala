package Level03

import scala.annotation.tailrec

object Problem0069 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  def calcTotientMaximum(limit: Int): Int =
    @tailrec
    def loop(acc: Int, n: Int): Int =
      if isPrime(n) then
        val nextProduct: Int = acc * n
        if nextProduct > limit then acc
        else loop(nextProduct, n + 1)
      else loop(acc, n + 1)

    loop(1, 2)

  def main(args: Array[String]): Unit =
    val limit: Int = 1000000
    val result: Int = calcTotientMaximum(limit)
    println(result)
}
