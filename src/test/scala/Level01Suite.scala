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
}
