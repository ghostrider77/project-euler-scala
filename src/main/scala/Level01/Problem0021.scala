package Level01

import scala.annotation.tailrec

object Problem0021 {
  private def isPrime(n: Int): Boolean =
    if n == 2 then true
    else if n == 1 || n % 2 == 0 then false
    else
      val limit: Int = math.sqrt(n).toInt
      (3 to limit by 2).forall(k => n % k != 0)

  private def findLargestExponent(n: Int, p: Int): (Int, Int) =
    @tailrec
    def loop(m: Int, alpha: Int): (Int, Int) =
      if m % p != 0 then (m, alpha)
      else loop(m / p, alpha + 1)

    loop(n, 0)

  private def calcPrimeFactorization(n: Int, primes: List[Int]): Map[Int, Int] =
    @tailrec
    def loop(acc: Map[Int, Int], m: Int, ps: List[Int]): Map[Int, Int] =
      if m == 1 then acc
      else ps match
        case Nil => acc + (m -> 1)
        case p :: pss =>
          findLargestExponent(m, p) match
            case (_, 0) => loop(acc, m, pss)
            case (rest, alpha) => loop(acc + (p -> alpha), rest, pss)

    loop(Map.empty[Int, Int], n, primes)

  private def calcSumOfDivisors(factorization: Map[Int, Int]): Int =
    factorization.foldLeft(1){ case (acc, (p, alpha)) => acc * ((math.pow(p, alpha + 1).toInt - 1) / (p - 1)) }

  def sumOfAmicableNumbers(n: Int): Int =
    val limit: Int = math.sqrt(n).toInt
    val primes: List[Int] = 2 :: (3 to limit by 2).filter(isPrime).toList
    val aliquotSums: Vector[Int] =
      Vector.tabulate(n)(k => calcSumOfDivisors(calcPrimeFactorization(k + 1, primes)) - (k + 1))
    (for {
      a <- 3 to n
      b <- 2 until a
      if aliquotSums(b - 1) == a && aliquotSums(a - 1) == b
    } yield a + b).sum

  def main(args: Array[String]): Unit =
    val limit: Int = 10000
    val result: Int = sumOfAmicableNumbers(limit)
    println(result)
}
