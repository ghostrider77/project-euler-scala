package Level01

object Problem0002 {
  def sumOfEvenFibonacciNumbers(n: Int): Int =
    val fibonacciNumbers: Iterator[Int] = Iterator.iterate((1, 2)){ case (a, b) => (b, a + b) }.map{ case (a, _) => a }
    fibonacciNumbers.filter(_ % 2 == 0).takeWhile(_ < n).sum

  def main(args: Array[String]): Unit =
    val n: Int = 4_000_000
    val result: Int = sumOfEvenFibonacciNumbers(n)
    println(result)
}
