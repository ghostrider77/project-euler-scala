package Level01

import scala.annotation.tailrec

object Problem0007 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  def getNthPrime(n: Int): Int =
    @tailrec
    def loop(k: Int, count: Int): Int =
      if isPrime(k) then
        val nextCount: Int = count + 1
        if nextCount == n then k
        else loop(k + 2, nextCount)
      else loop(k + 2, count)

    if n == 1 then 2
    else loop(3, 1)

  def main(args: Array[String]): Unit =
    val n: Int = 10001
    val result: Int = getNthPrime(n)
    println(result)
}
