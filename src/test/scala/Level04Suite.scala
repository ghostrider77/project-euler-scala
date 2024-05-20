import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class Level04Suite extends AnyFreeSpec with Matchers {
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

  "Arranged Probability" - {
    "should find the number of blue disk where the probability of taking two blue discs at random is 50%" in {
      import Level04.Problem0100.solveNegativePellEquation

      solveNegativePellEquation(21L) shouldEqual 15L
      solveNegativePellEquation(22L) shouldEqual 85L
    }
  }
}
