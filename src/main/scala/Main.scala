import Helpers.{
  mostOccuring,
  nextMove,
  previousMove,
  random,
  randomIn,
  score,
  whoBeats
}
import Moves.{PAPER, ROCK, SCISSORS}
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

  def parse(raw: String): Option[Moves.Value] =
    raw match {
      case "rock"     => Some(Moves.ROCK)
      case "paper"    => Some(Moves.PAPER)
      case "scissors" => Some(Moves.SCISSORS)
      case "None"     => None
      case _          => throw new IllegalArgumentException("Invalid move " + raw)
    }

  var strategiesScores = allStrategies.map(_ -> 0.toDouble).toMap

  var opponentHistory = List[Moves.Value]()
  var myHistory = List[Moves.Value]()

  while (true) {
    val maybePreviousOpponentMove = parse(readLine)

    opponentHistory = opponentHistory ++ maybePreviousOpponentMove

    val myMove = (opponentHistory, myHistory) match {
      case (h, mh) if h.length > 3 && mh.length > 3 =>
        strategiesScores = strategiesScores.map { case (strategy, score) =>
          (strategy, score + strategy.getScore(h, mh))
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
        randomIn(Moves.values)
    }

    myHistory = myHistory :+ myMove

    println(myMove.toString)
  }
}

object Moves extends Enumeration {
  type Move = Value
  val ROCK, PAPER, SCISSORS = Value
}

trait Strategy {
  def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double

  def move(
      previousOpponentMove: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value
}

class RandomStrategy extends Strategy {
  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    randomIn(Moves.values)

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = 0
}

class RockStrategy extends Strategy {
  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    Moves.ROCK

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double =
    score(Moves.ROCK, opponentHistory.last)
}

class PaperStrategy extends Strategy {
  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    Moves.PAPER

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double =
    score(Moves.PAPER, opponentHistory.last)
}

class ScissorsStrategy extends Strategy {
  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    Moves.SCISSORS

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double =
    score(Moves.SCISSORS, opponentHistory.last)
}

class CopyOpponentStrategy extends Strategy {
  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    opponentHistory.last

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class BeatLastOpponentStrategy extends Strategy {
  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    whoBeats(opponentHistory.last)

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategyAll extends Strategy {

  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    whoBeats(mostOccuring(opponentHistory))

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategy1 extends Strategy {

  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    whoBeats(mostOccuring(opponentHistory.take(1)))

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init.take(1), myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategy2 extends Strategy {

  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    whoBeats(mostOccuring(opponentHistory.take(2)))

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init.take(2), myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategy3 extends Strategy {

  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    whoBeats(mostOccuring(opponentHistory.take(3)))

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init.take(3), myHistory.init),
    opponentHistory.last
  )
}

class BeatMostOpponentStrategy4 extends Strategy {

  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    whoBeats(mostOccuring(opponentHistory.take(4)))

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init.take(4), myHistory.init),
    opponentHistory.last
  )
}

class AloneCircleClockwiseStrategy extends Strategy {

  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value =
    nextMove(myHistory.last)

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class AloneCircleCounterClockwiseStrategy extends Strategy {

  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value = previousMove(myHistory.last)

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class OpponentAloneCircleClockwiseStrategy extends Strategy {

  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value = whoBeats(nextMove(opponentHistory.last))

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class OpponentAloneCircleCounterClockwiseStrategy extends Strategy {

  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value = whoBeats(previousMove(opponentHistory.last))

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Double = score(
    this.move(opponentHistory.init, myHistory.init),
    opponentHistory.last
  )
}

class LearnerStrategy extends Strategy {
  override def move(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
  ): Moves.Value = {
    val (prediction, score) = RPSLearner.predict(opponentHistory)
    System.err.println(s"Prediction: $prediction, score: $score")
    whoBeats(prediction)
  }

  override def getScore(
      opponentHistory: List[Moves.Value],
      myHistory: List[Moves.Value]
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

  def score(me: Moves.Value, opponent: Moves.Value): Double = {
    (me, opponent) match {
      case (Moves.ROCK, Moves.ROCK)         => 0
      case (Moves.ROCK, Moves.PAPER)        => -1
      case (Moves.ROCK, Moves.SCISSORS)     => 1
      case (Moves.PAPER, Moves.ROCK)        => 1
      case (Moves.PAPER, Moves.PAPER)       => 0
      case (Moves.PAPER, Moves.SCISSORS)    => -1
      case (Moves.SCISSORS, Moves.ROCK)     => -1
      case (Moves.SCISSORS, Moves.PAPER)    => 1
      case (Moves.SCISSORS, Moves.SCISSORS) => 0
    }
  }

  def whoBeats(move: Moves.Value): Moves.Value = nextMove(move)

  def mostOccuring(moves: List[Moves.Value]): Moves.Value =
    moves
      .groupBy(identity)
      .map { case (move, moves) =>
        (move, moves.length)
      }
      .maxBy(_._2)
      ._1

  def nextMove(last: Moves.Value): Moves.Value = {
    last match {
      case Moves.ROCK     => Moves.PAPER
      case Moves.PAPER    => Moves.SCISSORS
      case Moves.SCISSORS => Moves.ROCK
    }
  }

  def previousMove(last: Moves.Value): Moves.Value = {
    last match {
      case Moves.ROCK     => Moves.SCISSORS
      case Moves.PAPER    => Moves.ROCK
      case Moves.SCISSORS => Moves.PAPER
    }
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

  def incrementNode(tree: Tree, nodePath: List[Moves.Value]): Unit = {
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

  def buildHistoryTree(history: List[Moves.Value]): Tree = {
    val allCombinations = getAllCombinations(history)
//    System.err.println("all combinations" + allCombinations)
    allCombinations.foldLeft(Tree(0, None, None, None)) {
      case (accTree, currentSubList) =>
        incrementNode(accTree, currentSubList)
        accTree
    }
  }

  def mapSum[K: Ordering](a: Map[K, Int], b: Map[K, Int]): Map[K, Int] = {
    (a.keySet ++ b.keySet).map { key =>
      (key, a.getOrElse(key, 0) + b.getOrElse(key, 0))
    }.toMap
  }

  def predictFromTree(
      tree: Tree,
      history: List[Moves.Value]
  ): (Moves.Value, Double) = {
    val nodesToEval = getAllCombinationsEnding(history)

//    System.err.println("nodes to eval: " + nodesToEval)

    val initialMap = Map(Moves.ROCK -> 0, Moves.PAPER -> 0, Moves.SCISSORS -> 0)
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

  def getNodeByPath(tree: Tree, path: List[Moves.Value]) =
    path.foldLeft(tree) {
      case (acc, ROCK)     => acc.r.getOrElse(Tree(0, None, None, None))
      case (acc, PAPER)    => acc.p.getOrElse(Tree(0, None, None, None))
      case (acc, SCISSORS) => acc.s.getOrElse(Tree(0, None, None, None))
    }

  def predict(history: List[Moves.Value]): (Moves.Value, Double) = {
    System.err.println(
      "history " + history
        .map {
          case Moves.ROCK     => "r"
          case Moves.PAPER    => "p"
          case Moves.SCISSORS => "s"
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
