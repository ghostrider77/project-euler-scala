import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Level03Suite extends AnyFreeSpec with Matchers {
  "Prime Digit Replacements" - {
    "should find the smallest prime from an eight prime value family" in {
      import Level03.Problem0051.smallestPrimeFromASameDigitReplacementFamily

      smallestPrimeFromASameDigitReplacementFamily(6) shouldEqual 13
    }
  }

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

  "Cubic Permutations" - {
    "should find the smallest cube for which exactly five permutations of its digits are cube" in {
      import Level03.Problem0062.findSmallestCube

      findSmallestCube(3) shouldEqual 41063625L
    }
  }

  "Odd Period Square Roots" - {
    "should count the numbers with and odd length of continued fraction expansion of their square roots" in {
      import Level03.Problem0064.calcNrOfOddPeriodSquareRootConvergents

      calcNrOfOddPeriodSquareRootConvergents(13) shouldEqual 4
    }
  }

  "Maximum Path Sum II" - {
    "should find the maximum total from top to bottom of a triangle grid" in {
      import Level03.Problem0067.calcMaxPathSum

      val grid: Vector[Vector[Int]] = Vector(Vector(3), Vector(7, 4), Vector(2, 4, 6), Vector(8, 5, 9, 3))
      calcMaxPathSum(grid) shouldEqual 23
    }
  }

  "Totient Maximum" - {
    "should calculate n for which n / phi(n) is the largest" in {
      import Level03.Problem0069.calcTotientMaximum

      calcTotientMaximum(10) shouldEqual 6
    }
  }

  "Ordered Fractions" - {
    "should find the numerator of the fraction immediately to the left of 3/7 for denominators up to a given limit" in {
      import Level03.Problem0071.orderedFractions

      val limit: Int = 8
      orderedFractions(limit) shouldEqual 2
    }
  }

  "Counting Fractions" - {
    "should calculate the number of proper reduced fractions" in {
      import Level03.Problem0072.nrProperReducedFractions

      nrProperReducedFractions(8) shouldEqual 21
    }
  }

  "Counting Fractions in a Range" - {
    "should retrieve the number of fractions in the sorted set of reduced proper fractions between 1/3 and 1/2" in {
      import Level03.Problem0073.calcNrReducedFractionsInRange

      calcNrReducedFractionsInRange(8) shouldEqual 3
    }
  }

  "Digit Factorial Chains" - {
    "should retrieve the length of the non-repeating chain" in {
      import Level03.Problem0074.calcFactorialChainLength

      calcFactorialChainLength(145) shouldEqual 1
      calcFactorialChainLength(69) shouldEqual 5
    }
  }

  "Singular Integer Right Triangles" - {
    "should retrieve the number of perimeters where a unique integer sided right angle triangle be formed" in {
      import Level03.Problem0075.singularIntegerRightTriangle

      singularIntegerRightTriangle(50) shouldEqual 6
    }
  }
}
