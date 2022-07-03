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
      RPSLearner.getAllCombinations(List(1, 2, 3)) === List(
        List(1),
        List(1, 2),
        List(1, 2, 3),
        List(2),
        List(2, 3),
        List(3)
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
        Map(Moves.ROCK -> Tree(1, Map(Moves.PAPER -> Tree(1, Map.empty))))
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
}
