import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Level01Suite extends AnyFreeSpec with Matchers {

  "Multiples of 3 or 5" - {
    "should find the sum of all the multiples of 3 or 5 below limit" in {
      import Level01.Problem0001.calcSumOfMultiples

      calcSumOfMultiples(10) shouldEqual 23
    }
  }

  "Even Fibonacci numbers" - {
    "should find the sum of the even-valued Fibonacci numbers not exceeding a given limit" in {
      import Level01.Problem0002.sumOfEvenFibonacciNumbers

      sumOfEvenFibonacciNumbers(100) shouldEqual 44
    }
  }

  "Largest Prime Factor" - {
    "should find the largest prime factor" in {
      import Level01.Problem0003.findLargestPrimeFactor

      findLargestPrimeFactor(13195) shouldEqual 29
    }
  }

  "Largest Palindrome Product" - {
    "should find the largest palindrome made from the product of two digit numbers" in {
      import Level01.Problem0004.largestPalindromeProduct

      largestPalindromeProduct(2) shouldEqual 9009
    }
  }

  "Smallest Multiple" - {
    "should calculate the smallest multiple of the numbers from 1 to n" in {
      import Level01.Problem0005.calcSmallestMultiple

      calcSmallestMultiple(10) shouldEqual 2520
    }
  }

  "Sum Square Difference" - {
    "should calculate the sum of the squares of the first n natural numbers and the square of the sum" in {
      import Level01.Problem0006.sumSquareDifference

      sumSquareDifference(10) shouldEqual 2640
    }
  }

  "10001st Prime" - {
    "should calculate the nth prime number" in {
      import Level01.Problem0007.getNthPrime

      getNthPrime(6) shouldEqual 13
    }
  }

  "Largest Product in a Series" - {
    "should calculate the maximum product of n adjacent digits" in {
      import Level01.Problem0008.calcLargestProduct

      val n: List[Int] = List(1, 4, 9, 3, 2, 8, 6, 8, 4)
      calcLargestProduct(n, windowSize = 2) shouldEqual 48
      calcLargestProduct(n, windowSize = 3) shouldEqual 384
    }
  }

  "Special Pythagorean Triplet" - {
    "should calculate the product of such triplet with a given sum" in {
      import Level01.Problem0009.calcPythagoreanTripletProduct

      calcPythagoreanTripletProduct(12) shouldEqual 60
    }
  }

  "Summation of Primes" - {
    "should calculate the sum of primes under a given limit" in {
      import Level01.Problem0010.sumOfPrimes

      sumOfPrimes(10) shouldEqual 17
    }
  }

  "Largest Product in a Grid" - {
    "should calculate greatest product of k adjacent numbers in the same direction" in {
      import Level01.Problem0011.{Grid, calcMaximalGridProduct}

      val grid = Grid(Vector(Vector(1, 2, 3), Vector(4, 1, 1), Vector(1, 0, 1)), 3, 3)
      calcMaximalGridProduct(grid, 2) shouldEqual 8
    }
  }

  "Highly Divisible Triangular Number" - {
    "should find the value of the first triangle number to have over k divisors" in {
      import Level01.Problem0012.findTriangleNumber

      findTriangleNumber(5) shouldEqual 28
    }
  }

  "Large Sum" - {
    "should calculate the first ten digits of a sum" in {
      import Level01.Problem0013.calcLargeSum

      val xs: List[BigInt] = List(BigInt(99), BigInt(89))
      calcLargeSum(xs, 2) shouldEqual "18"

    }
  }

  "Longest Collatz Sequence" - {
    "should find the length of a Collatz-sequence" in {
      import Level01.Problem0014.collatzSequenceLength

      collatzSequenceLength(13) shouldEqual 10
    }
  }

  "Lattice Paths" - {
    "should calculate the number of down/right routes on a square grid" in {
      import Level01.Problem0015.nChooseK

      nChooseK(4, 2) shouldEqual 6
    }
  }

  "Power Digit Sum" - {
    "should calculate the sum of the digits of the number 2 ** n" in {
      import Level01.Problem0016.powerDigitSum

      powerDigitSum(15) shouldEqual 26
    }
  }

  "Maximum Path Sum I" - {
    "should find the maximum total from top to bottom of a triangle grid" in {
      import Level01.Problem0018.maxPathSum

      val grid: Vector[Vector[Int]] = Vector(Vector(3), Vector(7, 4), Vector(2, 4, 6), Vector(8, 5, 9, 3))
      maxPathSum(grid) shouldEqual 23
    }
  }

  "Factorial Digit Sum" - {
    "should find the sum of the digits in the number 100!" in {
      import Level01.Problem0020.factorialDigitSum

      val n: Int = 10
      factorialDigitSum(10) shouldEqual 27
    }
  }

  "Amicable Numbers" - {
    "should calculate the sum of all the amicable numbers under a given limit" in {
      import Level01.Problem0021.sumOfAmicableNumbers

      sumOfAmicableNumbers(284) shouldEqual 504
    }
  }

  "Names Scores" - {
    "should calculate the total of all the name scores" in {
      import Level01.Problem0022.calcNameScores

      calcNameScores(List("COLIN")) shouldEqual 53
    }
  }

  "Non-Abundant Sums" - {
    "should find the sum of all the positive integers which cannot be written as the sum of two abundant numbers" in {
      import Level01.Problem0023.sumOfNonAbundantSums

      val expectedSum: Int = 23 * 24 / 2
      sumOfNonAbundantSums(23) shouldEqual expectedSum
      sumOfNonAbundantSums(24) shouldEqual expectedSum
    }
  }

  "Lexicographic Permutations" - {
    "should calculate the millionth lexicographic permutation of the digits from 0 to 9" in {
      import Level01.Problem0024.calcNthPermutation

      val n: Int = 5
      val digits: List[Int] = List(0, 1, 2)
      calcNthPermutation(n, digits) shouldEqual List(2, 0, 1)
    }
  }

  "1000-digit Fibonacci Number" - {
    "should find the index of the first term in the Fibonacci sequence to contain k digits" in {
      import Level01.Problem0025.getIndexOfLargeFibonacciNumber

      getIndexOfLargeFibonacciNumber(3) shouldEqual 12
    }
  }
}
