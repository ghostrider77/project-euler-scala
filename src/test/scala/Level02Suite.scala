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
}
