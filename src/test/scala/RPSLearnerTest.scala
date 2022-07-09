import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpec

class RPSLearnerTest extends AnyWordSpec with TableDrivenPropertyChecks {
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
      assert(result === Tree(0, None, None, None))
    }

    "work with single history" in {
      val result = RPSLearner.buildHistoryTree(List(ROCK))
      assert(result === Tree(0, Some(Tree(1, None, None, None)), None, None))
    }

    "work with history size 2" in {
      val result = RPSLearner.buildHistoryTree(List(ROCK, PAPER))
      val expected = Tree(
        0,
        Some(Tree(1, None, Some(Tree(1, None, None, None)), None)),
        Some(Tree(1, None, None, None)),
        None
      )
      assert(result === expected)
    }

    "work with RPSRPS" in {
      val result = RPSLearner.buildHistoryTree(
        List(
          ROCK,
          PAPER,
          SCISSORS,
          ROCK,
          PAPER,
          SCISSORS
        )
      )
      val expected = Tree(
        0,
        Some(
          Tree(
            2,
            None,
            Some(
              Tree(
                2,
                None,
                None,
                Some(
                  Tree(
                    2,
                    Some(
                      Tree(
                        1,
                        None,
                        Some(
                          Tree(1, None, None, Some(Tree(1, None, None, None)))
                        ),
                        None
                      )
                    ),
                    None,
                    None
                  )
                )
              )
            ),
            None
          )
        ),
        Some(
          Tree(
            2,
            None,
            None,
            Some(
              Tree(
                2,
                Some(
                  Tree(
                    1,
                    None,
                    Some(Tree(1, None, None, Some(Tree(1, None, None, None)))),
                    None
                  )
                ),
                None,
                None
              )
            )
          )
        ),
        Some(
          Tree(
            2,
            Some(
              Tree(
                1,
                None,
                Some(Tree(1, None, None, Some(Tree(1, None, None, None)))),
                None
              )
            ),
            None,
            None
          )
        )
      )
      assert(result === expected)
    }
  }

  "increment node" should {
    "work" in {
      val tree = Tree(0, None, None, None)
      RPSLearner.incrementNode(tree, List(ROCK))
      val expected = Tree(0, Some(Tree(1, None, None, None)), None, None)
      assert(tree === expected)
    }

    "work with new branch" in {
      val tree = Tree(0, None, None, None)
      RPSLearner.incrementNode(tree, List(ROCK, PAPER))
      val expected = Tree(
        0,
        Some(Tree(0, None, Some(Tree(1, None, None, None)), None)),
        None,
        None
      )
      assert(tree === expected)
    }
  }

  def parseInput(raw: String) = raw.split("").toList.map(_.toUpperCase).map {
    case "R" => ROCK
    case "P" => PAPER
    case "S" => SCISSORS
  }

  "predict" should {
    "work with simple cases" in {
      val cases = Table(
        ("input", "expected", "probability"),
        ("rpsrpsrps", ROCK, 1.0),
        ("psrpsrpsr", PAPER, 1.0),
        ("srpsrpsrp", SCISSORS, 1.0),
        ("rrppssrrppssr", ROCK, 0.8),
        ("rrrrrrrrrr", ROCK, 1),
        ("pppppppppp", PAPER, 1),
        ("ssssssssss", SCISSORS, 1)
      )

      forAll(cases) { case (input, expected, probability) =>
        val result = RPSLearner.predict(parseInput(input))
        assert(result._1 === expected)
        assert(result._2 === probability)
      }
    }

    "long test" in {
      for (i <- 1 to 1000) {

        val res = RPSLearner.predict(
          parseInput(
            "ssrrsrspsspprsrssrrsrpprrppssspsrsspppsprpsrpsrsspssprrppsssrrprpppspspsrsprpsrppprprsprpprrrrsrprrpsrsssprrrsrpss"
          )
        )
        assert(res === (PAPER, 0.36363636363636365))
      }
    }
  }
}
