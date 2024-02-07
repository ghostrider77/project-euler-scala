import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Level02Suite extends AnyFreeSpec with Matchers {

  "Reciprocal Cycles" - {
    "should find the value of d for which 1/d contains the longest recurring cycle in its decimal fraction part" in {
      import Level02.Problem0026.calcNumberWithLongestRecurringCycle

      calcNumberWithLongestRecurringCycle(11) shouldEqual 7
    }
  }

  "Quadratic Primes" - {
    "should find the quadratic expression that produces the maximum number of primes for consecutive values of n" in {
      import Level02.Problem0027.calcPolynomicalWithLongestPrimeRun

      val limit: Int = 41
      math.abs(calcPolynomicalWithLongestPrimeRun(limit)) shouldEqual 41
    }
  }

  "Number Spiral Diagonals" - {
    "should calculate the sum of the numbers on the diagonals formed by a spiral" in {
      import Level02.Problem0028.calcDiagonalSums

      calcDiagonalSums(5) shouldEqual 101
    }
  }

  "Distinct Powers" - {
    "should calculate the number of distinct powers a^b where 2 <= a, b <= limit" in {
      import Level02.Problem0029.distinctPowers

      distinctPowers(5) shouldEqual 15
    }
  }

  "Digit Fifth Powers" - {
    "should find the sum of all the numbers that can be written as the sum of fifth powers of their digits" in {
      import Level02.Problem0030.calcSumOfDigitPowers

      calcSumOfDigitPowers(4) shouldEqual 19316
    }
  }

  "Coin Sums" - {
    "should calculate the number of different ways a 2 pound note can be changed" in {
      import Level02.Problem0031.coinSums

      val coins: List[Int] = List(5, 2, 1)
      val amount: Int = 3
      coinSums(amount, coins) shouldEqual 2
    }
  }

  "Digit Factorials" - {
    "should check if a number is the sum of the factorial of its digits" in {
      import Level02.Problem0034.isFactorialSumOfDigits

      isFactorialSumOfDigits(145) shouldBe true
    }
  }

  "Circular Primes" - {
    "should count the number of circular primes, that is, each circular shift of its digits is a prime below N" in {
      import Level02.Problem0035.calcNumberOfCircularPrimes

      calcNumberOfCircularPrimes(100) shouldEqual 13
    }
  }

  "Double-base Palindromes" - {
    "should find numbers that are palindromic both in base 10 and 2" in {
      import Level02.Problem0036.isPalindrome

      val n: Int = 585
      isPalindrome(n, base = 10) shouldBe true
      isPalindrome(n, base = 2) shouldBe true
    }
  }

  "Integer Right Triangles" - {
    "should find the perimeter length such that the number of Pythagorean triples are maximized" in {
      import Level02.Problem0039.calcPerimeterWithMostPythagoreanTriples

      val limit: Int = 130
      calcPerimeterWithMostPythagoreanTriples(limit) shouldEqual 120
    }
  }

  "Champernowne's Constant" - {
    "should find the digits of Champernowne's constant" in {
      import Level02.Problem0040.champernownesConstantProduct

      champernownesConstantProduct(List(12)) shouldEqual 1
    }
  }

  "Pandigital Prime" - {
    "should find the largest k-digit pandigital prime" in {
      import Level02.Problem0041.largestPandigitalPrime

      val limit: Int = 2200
      largestPandigitalPrime(limit) shouldEqual 2143
    }
  }

  "Coded Triangle Numbers" - {
    "should find the number of encoded triangle numbers" in {
      import Level02.Problem0042.calcNumberOfTriangleWords

      val words: List[String] = List("A", "B", "D", "SKY")
      calcNumberOfTriangleWords(words) shouldEqual 2
    }
  }

  "Triangular, Pentagonal, and Hexagonal" - {
    "should find the next triangle number that is also pentagonal and hexagonal" in {
      import Level02.Problem0045.findHexagonalPentagonalNumber

      findHexagonalPentagonalNumber(1, 1) shouldEqual 40755
    }
  }

  "Distinct Primes Factors" - {
    "should find the first k consecutive integers to have four distinct prime factors each" in {
      import Level02.Problem0047.distinctPrimeFactors

      val k: Int = 2
      distinctPrimeFactors(k) shouldEqual 14
    }
  }

  "Self Powers" - {
    "should find the last ten digits of the series 1^1 + 2^2 + ... + 1000^1000" in {
      import Level02.Problem0048.lastDigitOfSelfPowerSum

      lastDigitOfSelfPowerSum(10) shouldEqual "0405071317"
    }
  }

  "Consecutive Prime Sum" - {
    "should find the prime below a given limit that can be written as the sum of the most consecutive primes" in {
      import Level02.Problem0050.sumOfConsecutivePrimes

      sumOfConsecutivePrimes(100) shouldEqual 41
      sumOfConsecutivePrimes(1000) shouldEqual 953
    }
  }
}
