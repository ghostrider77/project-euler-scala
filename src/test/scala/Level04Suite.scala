import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Level04Suite extends AnyFreeSpec with Matchers {
  "Counting Summations" - {
    "should find the number of ways `n` be written as a sum of at least two positive integers" in {
      import Level04.Problem0076.calcNumberOfSums

      val n: Int = 5
      calcNumberOfSums(n) shouldEqual 6
    }
  }

  "Prime Summations" - {
    "should find the first value which can be written as the sum of primes in over `k` different ways" in {
      import Level04.Problem0077.primeSummation

      val k: Int = 4
      primeSummation(k) shouldEqual 10
    }
  }

  "Su Doku" - {
    "should find the top left 3-digit value for a uniquely solvable sudoku puzzle" in {
      import Level04.Problem0096.{Puzzle, readPuzzle, solveSudokuPuzzles}

      val lines: List[String] = List(
        "003020600",
        "900305001",
        "001806400",
        "008102900",
        "700000008",
        "006708200",
        "002609500",
        "800203009",
        "005010300"
      )
      val puzzle: Puzzle = readPuzzle(lines)
      solveSudokuPuzzles(List(puzzle)) shouldEqual 483
    }
  }

  "Largest Exponential" - {
    "should find the 1-based index of the largest exponential" in {
      import Level04.Problem0099.{Number, findIndexOfLargestNumber}

      val numbers: List[Number] = List(Number(2, 3), Number(3, 2), Number(8, 1))
      findIndexOfLargestNumber(numbers) shouldEqual 2
    }
  }

  "Arranged Probability" - {
    "should find the number of blue disk where the probability of taking two blue discs at random is 50%" in {
      import Level04.Problem0100.solveNegativePellEquation

      solveNegativePellEquation(21L) shouldEqual 15L
      solveNegativePellEquation(22L) shouldEqual 85L
    }
  }
}
