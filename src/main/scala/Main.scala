import Helpers.{
  mostOccuring,
  nextMove,
  previousMove,
  random,
  randomIn,
  score,
  whoBeats
}
import Player.opponentHistory

import scala.+:
import scala.collection.mutable.ListBuffer
import scala.util._
import scala.io.StdIn._

object Player extends App {
  val allStrategies: Seq[Strategy] = Seq(
    new RandomStrategy,
    new RockStrategy,
    new PaperStrategy,
    new ScissorsStrategy,
    new CopyOpponentStrategy,
    new BeatLastOpponentStrategy,
    new BeatMostOpponentStrategyAll,
    new BeatMostOpponentStrategy1,
    new BeatMostOpponentStrategy2,
    new BeatMostOpponentStrategy3,
    new AloneCircleClockwiseStrategy,
    new AloneCircleCounterClockwiseStrategy,
    new OpponentAloneCircleClockwiseStrategy,
    new OpponentAloneCircleCounterClockwiseStrategy,
    new LearnerStrategy
  )

  def parse(raw: String): Option[Move] =
    raw match {
      case "rock"     => Some(ROCK)
      case "paper"    => Some(PAPER)
      case "scissors" => Some(SCISSORS)
      case "None"     => None
      case _          => throw new IllegalArgumentException("Invalid move " + raw)
    }

  var strategiesScores = allStrategies.map(_ -> 0.toDouble).toMap

  var opponentHistory = List[Move]()
  var myHistory = List[Move]()

  while (true) {
    val maybePreviousOpponentMove = parse(readLine)

    opponentHistory = opponentHistory ++ maybePreviousOpponentMove

    val myMove = (opponentHistory, myHistory) match {
      case (h, mh) if h.length > 3 && mh.length > 3 =>
        strategiesScores = strategiesScores.map { case (strategy, score) =>
          (
            strategy,
            score + strategy.getScore(h, mh)
          )
        }

        System.err.println(s"Strategies scores (${strategiesScores.size})")
        System.err.println(strategiesScores)

        strategiesScores.toList.sortBy(_._2).reverse.foreach {
          case (strategy, score) =>
            System.err.println(s"${strategy.getClass.getSimpleName}: $score")
        }

        strategiesScores.maxBy(_._2) match {
          case (strategy, score) =>
            System.err.println(
              s"Best strategy is ${strategy.getClass.getSimpleName} with score $score"
            )
            strategy.move(h, mh)
        }
      case other =>
        System.err.println("First turn")
        randomIn(Move.values)
    }

    myHistory = myHistory :+ myMove

    println(myMove.toString)
  }
}

trait Move
case object ROCK extends Move
case object PAPER extends Move
case object SCISSORS extends Move

object Move {
  val values = Seq(ROCK, PAPER, SCISSORS)
}

trait Strategy {
  def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double

  def move(
      previousOpponentMove: List[Move],
      myHistory: List[Move]
  ): Move
}

class RandomStrategy extends Strategy {
  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    randomIn(Move.values)

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = 0
}

class RockStrategy extends Strategy {
  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    ROCK

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double =
    score(ROCK, opponentHistory.last)
}

class PaperStrategy extends Strategy {
  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    PAPER

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double =
    score(PAPER, opponentHistory.last)
}

class ScissorsStrategy extends Strategy {
  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    SCISSORS

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double =
    score(SCISSORS, opponentHistory.last)
}

class CopyOpponentStrategy extends Strategy {
  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    opponentHistory.last

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class BeatLastOpponentStrategy extends Strategy {
  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    whoBeats(opponentHistory.last)

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategyAll extends Strategy {

  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    whoBeats(mostOccuring(opponentHistory))

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategy1 extends Strategy {

  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    whoBeats(mostOccuring(opponentHistory.take(1)))

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init.take(1), myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategy2 extends Strategy {

  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    whoBeats(mostOccuring(opponentHistory.take(2)))

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init.take(2), myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategy3 extends Strategy {

  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    whoBeats(mostOccuring(opponentHistory.take(3)))

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init.take(3), myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategy4 extends Strategy {

  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    whoBeats(mostOccuring(opponentHistory.take(4)))

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init.take(4), myHistory.init),
    opponentHistory.last
  )
}

class AloneCircleClockwiseStrategy extends Strategy {

  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move =
    nextMove(myHistory.last)

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class AloneCircleCounterClockwiseStrategy extends Strategy {

  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move = previousMove(myHistory.last)

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class OpponentAloneCircleClockwiseStrategy extends Strategy {

  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move = whoBeats(nextMove(opponentHistory.last))

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class OpponentAloneCircleCounterClockwiseStrategy extends Strategy {

  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move = whoBeats(previousMove(opponentHistory.last))

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class LearnerStrategy extends Strategy {
  override def move(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Move = {
    val (prediction, score) = RPSLearner.predict(opponentHistory)
    System.err.println(s"Prediction: $prediction, score: $score")
    whoBeats(prediction)
  }

  override def getScore(
      opponentHistory: List[Move],
      myHistory: List[Move]
  ): Double =
    score(
      this.move(opponentHistory.init, myHistory.init),
      opponentHistory.last
    )
}

object Helpers {
  val random = new Random()

  def randomIn[T](values: Iterable[T]): T =
    values.toList(random.nextInt(values.size))

  def score(me: Move, opponent: Move): Double = {
    (me, opponent) match {
      case (ROCK, ROCK)         => 0
      case (ROCK, PAPER)        => -1
      case (ROCK, SCISSORS)     => 1
      case (PAPER, ROCK)        => 1
      case (PAPER, PAPER)       => 0
      case (PAPER, SCISSORS)    => -1
      case (SCISSORS, ROCK)     => -1
      case (SCISSORS, PAPER)    => 1
      case (SCISSORS, SCISSORS) => 0
    }
  }

  def whoBeats(move: Move): Move = nextMove(move)

  def mostOccuring(moves: List[Move]): Move =
    moves
      .groupBy(identity)
      .map { case (move, moves) =>
        (move, moves.length)
      }
      .maxBy(_._2)
      ._1

  def nextMove(last: Move): Move = {
    last match {
      case ROCK     => PAPER
      case PAPER    => SCISSORS
      case SCISSORS => ROCK
    }
  }

  def previousMove(last: Move): Move = last match {
    case ROCK     => SCISSORS
    case PAPER    => ROCK
    case SCISSORS => PAPER
  }
}

object RPSLearner {
  def main(args: Array[String]): Unit = {}

  /** All sub lists that match the end of a list.
    * Exemple:
    * - Input: List(1, 2, 3, 4, 5)
    * - Output: List(List(1, 2, 3, 4, 5), List(2, 3, 4, 5), List(3, 4, 5), List(4, 5), List(5))
    */
  def getAllCombinationsEnding[T](list: List[T]): List[List[T]] = {
    list match {
      case Nil            => Nil
      case ::(head, next) => (head +: next) +: getAllCombinationsEnding(next)
    }
  }

  /** All sub lists that are contained in list.
    * Exemple:
    * - Input: List(1, 2, 3, 4, 5)
    * - Output: List(List(1), List(1, 2), List(1, 2, 3), List(1, 2, 3, 4), List(1, 2, 3, 4, 5), List(2), List(2, 3), List(2, 3, 4), List(2, 3, 4, 5), List(3), List(3, 4), List(3, 4, 5), List(4), List(4, 5), List(5))
    */
  def getAllCombinations[T](list: List[T]): List[List[T]] = {
    var res = ListBuffer[List[T]]()
    for (from <- 0 to list.length) {
      for (to <- from + 1 to list.length) {
        res += list.slice(from, to)
      }
    }
    res.toList
  }

  def incrementNode(tree: Tree, nodePath: List[Move]): Unit = {
    nodePath match {
      case Nil =>
        tree.count += 1
      case ROCK :: other =>
        if (tree.r.isEmpty) {
          tree.r = Some(Tree(0, None, None, None))
        }
        incrementNode(tree.r.get, other)
      case PAPER :: other =>
        if (tree.p.isEmpty) {
          tree.p = Some(Tree(0, None, None, None))
        }
        incrementNode(tree.p.get, other)
      case SCISSORS :: other =>
        if (tree.s.isEmpty) {
          tree.s = Some(Tree(0, None, None, None))
        }
        incrementNode(tree.s.get, other)
    }
  }

  def buildHistoryTree(history: List[Move]): Tree = {
    val allCombinations = getAllCombinations(history)
//    System.err.println("all combinations" + allCombinations)
    allCombinations.foldLeft(Tree(0, None, None, None)) {
      case (accTree, currentSubList) =>
        incrementNode(accTree, currentSubList)
        accTree
    }
  }

  def mapSum[K](a: Map[K, Int], b: Map[K, Int]): Map[K, Int] = {
    (a.keySet ++ b.keySet).map { key =>
      (key, a.getOrElse(key, 0) + b.getOrElse(key, 0))
    }.toMap
  }

  def predictFromTree(
      tree: Tree,
      history: List[Move]
  ): (Move, Double) = {
    val nodesToEval = getAllCombinationsEnding(history)

//    System.err.println("nodes to eval: " + nodesToEval)

    val initialMap = Map(ROCK -> 0, PAPER -> 0, SCISSORS -> 0)
    val movesByCount = nodesToEval.foldLeft(initialMap) { case (acc, path) =>
      val node = getNodeByPath(tree, path)
      val childrenCount = Map(
        ROCK -> node.r.map(_.count).getOrElse(0),
        PAPER -> node.p.map(_.count).getOrElse(0),
        SCISSORS -> node.s.map(_.count).getOrElse(0)
      )
      mapSum(acc, childrenCount)
    }

    System.err.println("movesByCount: " + movesByCount)

    val total = movesByCount.values.sum

    val best = movesByCount.maxBy { case (move, count) =>
      count
    }

    (best._1, best._2.toDouble / total.toDouble)
  }

  def getNodeByPath(tree: Tree, path: List[Move]) =
    path.foldLeft(tree) {
      case (acc, ROCK)     => acc.r.getOrElse(Tree(0, None, None, None))
      case (acc, PAPER)    => acc.p.getOrElse(Tree(0, None, None, None))
      case (acc, SCISSORS) => acc.s.getOrElse(Tree(0, None, None, None))
    }

  def predict(history: List[Move]): (Move, Double) = {
    System.err.println(
      "history " + history
        .map {
          case ROCK     => "r"
          case PAPER    => "p"
          case SCISSORS => "s"
        }
        .mkString("")
    )
    val tree = buildHistoryTree(history)
    predictFromTree(tree, history)
  }
}

case class Tree(
    var count: Int,
    var r: Option[Tree],
    var p: Option[Tree],
    var s: Option[Tree]
)

object Tree {
  def makeNode(): Unit = Tree(0, None, None, None)
}
