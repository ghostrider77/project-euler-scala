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
}
