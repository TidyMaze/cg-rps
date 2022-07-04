import Moves.PAPER
import Moves.ROCK
import Moves.SCISSORS
import org.scalatest.wordspec.AnyWordSpec

class RPSLearnerTest extends AnyWordSpec {
  "getAllCombinationsEnding" should {
    "work for empty list" in {
      val result = RPSLearner.getAllCombinationsEnding(List.empty)
      assert(result.isEmpty)
    }

    "work for size 1" in {
      val result = RPSLearner.getAllCombinationsEnding(List(1))
      assert(result === List(List(1)))
    }

    "work for size 2" in {
      val result = RPSLearner.getAllCombinationsEnding(List(1, 2))
      assert(result === List(List(1, 2), List(2)))
    }

    "work for size 5" in {
      val result = RPSLearner.getAllCombinationsEnding(List(1, 2, 3, 4, 5))
      assert(
        result === List(
          List(1, 2, 3, 4, 5),
          List(2, 3, 4, 5),
          List(3, 4, 5),
          List(4, 5),
          List(5)
        )
      )
    }
  }

  "getAllCombinations" should {
    "work" in {
      assert(
        RPSLearner.getAllCombinations(List(1, 2, 3)) === List(
          List(1),
          List(1, 2),
          List(1, 2, 3),
          List(2),
          List(2, 3),
          List(3)
        )
      )
    }

    "work when repeated" in {
      assert(
        RPSLearner.getAllCombinations(List(1, 2, 1)) === List(
          List(1),
          List(1, 2),
          List(1, 2, 1),
          List(2),
          List(2, 1),
          List(1)
        )
      )
    }
  }

  "buildHistoryTree" should {
    "work with empty history" in {
      val result = RPSLearner.buildHistoryTree(List())
      assert(result === Tree(0, Map.empty))
    }

    "work with single history" in {
      val result = RPSLearner.buildHistoryTree(List(Moves.ROCK))
      assert(result === Tree(0, Map(Moves.ROCK -> Tree(1, Map.empty))))
    }

    "work with history size 2" in {
      val result = RPSLearner.buildHistoryTree(List(Moves.ROCK, Moves.PAPER))
      val expected = Tree(
        0,
        Map(
          PAPER -> Tree(1, Map()),
          ROCK -> Tree(1, Map(PAPER -> Tree(1, Map())))
        )
      )
      assert(result === expected)
    }

    "work with RPSRPS" in {
      val result = RPSLearner.buildHistoryTree(
        List(
          Moves.ROCK,
          Moves.PAPER,
          Moves.SCISSORS,
          Moves.ROCK,
          Moves.PAPER,
          Moves.SCISSORS
        )
      )
      val expected = Tree(
        0,
        Map(
          ROCK -> Tree(
            2,
            Map(
              PAPER -> Tree(
                2,
                Map(
                  SCISSORS -> Tree(
                    2,
                    Map(
                      ROCK -> Tree(
                        1,
                        Map(PAPER -> Tree(1, Map(SCISSORS -> Tree(1, Map()))))
                      )
                    )
                  )
                )
              )
            )
          ),
          PAPER -> Tree(
            2,
            Map(
              SCISSORS -> Tree(
                2,
                Map(
                  ROCK -> Tree(
                    1,
                    Map(PAPER -> Tree(1, Map(SCISSORS -> Tree(1, Map()))))
                  )
                )
              )
            )
          ),
          SCISSORS -> Tree(
            2,
            Map(
              ROCK -> Tree(
                1,
                Map(PAPER -> Tree(1, Map(SCISSORS -> Tree(1, Map()))))
              )
            )
          )
        )
      )
      assert(result === expected)
    }
  }

  "increment node" should {
    "work" in {
      val tree = Tree(0, Map(Moves.ROCK -> Tree(1, Map.empty)))
      val result = RPSLearner.incrementNode(tree, List(Moves.ROCK))
      val expected = Tree(0, Map(Moves.ROCK -> Tree(2, Map.empty)))
      assert(result === expected)
    }

    "work with new branch" in {
      val tree = Tree(0, Map(Moves.ROCK -> Tree(1, Map.empty)))
      val result = RPSLearner.incrementNode(tree, List(Moves.ROCK, Moves.PAPER))
      val expected = Tree(
        0,
        Map(Moves.ROCK -> Tree(1, Map(Moves.PAPER -> Tree(1, Map.empty))))
      )
      assert(result === expected)
    }
  }

  def parseInput(raw: String) = raw.split("").toList.map(_.toUpperCase).map {
    case "R" => Moves.ROCK
    case "P" => Moves.PAPER
    case "S" => Moves.SCISSORS
  }

  "predict" should {
    "work with simplest RPS loop" in {
      val input = parseInput("rpsrpsrps")
      assert(RPSLearner.predict(input) === (Moves.ROCK, 1.0))
    }
  }
}
