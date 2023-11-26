package Level01

import scala.annotation.tailrec
import scala.collection.mutable.Map as MutableMap

object Problem0012 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def extractPrimeFactor(n: Int, p: Int, factorization: MutableMap[Int, Int]): Int =
    @tailrec
    def loop(m: Int, exponent: Int): Int =
      if m % p == 0 then loop(m / p, exponent + 1)
      else
        if exponent > 0 then factorization(p) = exponent
        m

    loop(n, 0)

  private def calcPrimeFactorization(n: Int): Map[Int, Int] =
    val factorization: MutableMap[Int, Int] = MutableMap.empty[Int, Int]
    val m = extractPrimeFactor(n, 2, factorization)
    val limit: Int = math.sqrt(n).toInt
    val rest: Int = (3 to limit by 2).foldLeft(m)((acc, p) => extractPrimeFactor(acc, p, factorization))
    if rest > 1 then factorization(rest) = 1
    factorization.toMap

  private def calcNumberOfDivisors(n: Int): Int =
    val factorization: Map[Int, Int] = calcPrimeFactorization(n)
    factorization.values.foldLeft(1)((acc, exponent) => acc * (exponent + 1))

  def findTriangleNumber(nrDivisors: Int): Int =
    @tailrec
    def loop(n: Int): Int =
      val triangleNumber = n * (n + 1) / 2
      if calcNumberOfDivisors(triangleNumber) > nrDivisors then triangleNumber
      else loop(n + 1)

    loop(1)

  def main(args: Array[String]): Unit =
    val nrDivisors: Int = 500
    val result: Int = findTriangleNumber(nrDivisors)
    println(result)
}
