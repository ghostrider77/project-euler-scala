import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Level04Suite extends AnyFreeSpec with Matchers {
  object Data {
    val matrix: Vector[Vector[Int]] =
      Vector(
        Vector(131, 673, 234, 103, 18),
        Vector(201, 96, 342, 965, 150),
        Vector(630, 803, 746, 422, 111),
        Vector(537, 699, 497, 121, 956),
        Vector(805, 732, 524, 37, 331)
      )
  }

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

  "Coin Partitions" - {
    "should find the smallest value of `n` such that the number of partitions of `n` items is divisible by `m`" in {
      import Level04.Problem0078.calcSmallestValue

      val m: Int = 7
      calcSmallestValue(m) shouldEqual 5
    }
  }

  "Path Sum: Two Ways" - {
    "should find the minimal path sum by only moving right and down" in {
      import Level04.Problem0081.calcShortestPath
      import Data.matrix

      calcShortestPath(matrix) shouldEqual 2427
    }
  }

  "Path Sum: Three Ways" - {
    "should find the minimal path sum by only moving right, up and down" in {
      import Level04.Problem0082.{Grid, calcShortestPath}
      import Data.matrix

      val grid = Grid(matrix, matrix.length, matrix.head.length)
      calcShortestPath(grid) shouldEqual 994
    }
  }

  "Path Sum: Four Ways" - {
    "should find the minimal path sum for top left to bottom right corner by moving left, right, up and down" in {
      import Level04.Problem0083.{Grid, calcShortestPath}
      import Data.matrix

      val grid = Grid(matrix, matrix.length, matrix.head.length)
      calcShortestPath(grid) shouldEqual 2297
    }
  }

  "Cuboid Route" - {
    "should find the least value such that the number of solutions first exceeds one million" in {
      import Level04.Problem0086.smallestDimensionForCuboidRoutes

      val limit: Int = 2000
      smallestDimensionForCuboidRoutes(limit) shouldEqual 100
    }
  }

  "Prime Power Triples" - {
    "should find the numbers that can be expressed as the sum of a prime square, prime cube and prime fourth power" in {
      import Level04.Problem0087.numberOfPrimePowerSums

      val limit: Int = 50
      numberOfPrimePowerSums(limit) shouldEqual 4
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
