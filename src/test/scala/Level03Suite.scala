import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Level03Suite extends AnyFreeSpec with Matchers {
  "Combinatoric Selections" - {
    "should calculate the number of binomial coefficients exceeding a given threshold" in {
      import Level03.Problem0053.calcNumberOfLargeBinomialCoefficient

      val limit: Int = 10
      val n: Int = 6
      calcNumberOfLargeBinomialCoefficient(n, limit) shouldEqual 5
    }
  }

  "Poker Hands" - {
    "should calculate the number of hands that Player 1 wins" in {
      import Level03.Problem0054.{Game, readGame, nrGamesWon}

      val lines = Iterator(
        "5H 5C 6S 7S KD 2C 3S 8S 8D TD",
        "5D 8C 9S JS AC 2C 5C 7D 8S QH",
        "2D 9C AS AH AC 3D 6D 7D TD QD",
        "4D 6S 9H QH QC 3D 6D 7H QD QS",
        "2H 2D 4C 4D 4S 3C 3D 3S 9S 9D"
      )
      val games: Iterator[Game] = lines.map(readGame)
      nrGamesWon(games) shouldEqual 3
    }
  }

  "Lychrel Numbers" - {
    "should detect if a small integer is a Lychrel number" in {
      import Level03.Problem0055.isLychrelNumber

      isLychrelNumber(349) shouldBe false
      isLychrelNumber(196) shouldBe true
    }
  }

  "Powerful Digit Sum" - {
    "should calculate the number a^b having maximal digit sum" in {
      import Level03.Problem0056.calcMaximalDigitSum

      calcMaximalDigitSum(5) shouldEqual 13
    }
  }

  "Square Root Convergents" - {
    "should calculate the convergents of sqrt{2} where the nominator has more digits" in {
      import Level03.Problem0057.countConvergentsWithLargeNominators

      val limit: Int = 8
      countConvergentsWithLargeNominators(limit - 1) shouldEqual 0
      countConvergentsWithLargeNominators(limit) shouldEqual 1
    }
  }

  "Spiral Primes" - {
    "should return the side length of the spiral pattern when the ratio of primes in the diagonals are small" in {
      import Level03.Problem0058.diagonalPrimes

      diagonalPrimes(0.56) shouldEqual 5
    }
  }

  "Prime Pair Sets" - {
    "should find the lowest sum for a set of n primes for which any two primes produce another prime" in {
      import Level03.Problem0060.findPrimePairSetWithLowestSum

      findPrimePairSetWithLowestSum(4) shouldEqual 792
    }
  }
}