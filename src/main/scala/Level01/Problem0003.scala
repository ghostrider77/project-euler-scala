package Level01

import scala.annotation.tailrec

object Problem0003 {
  @tailrec
  private def removeFactor(n: Long, p: Long): Long =
    if n % p == 0 then removeFactor(n / p, p) else n

  def findLargestPrimeFactor(n: Long): Long =
    val limit: Long = math.floor(math.sqrt(n.toDouble)).toLong
    val divisiorCandidates: Iterator[Long] = Iterator.single(2L) ++ (3L to limit by 2)
    val (rest, largestFactor): (Long, Long) =
      divisiorCandidates.foldLeft((n, 1L)){ 
        case (acc @ (m, maxP), p) => if m % p != 0 then acc else (removeFactor(m, p), p) 
      }
    if rest > 1 then rest else largestFactor

  def main(args: Array[String]): Unit =
    val n: Long = 600851475143L
    val result: Long = findLargestPrimeFactor(n)
    println(result)
}
