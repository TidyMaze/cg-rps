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
}
