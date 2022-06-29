import Helpers.{random, randomIn, score, whoBeats}
import Player.opponentHistory

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
    new BeatMostOpponentStrategyAll
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

  var maybeMyLastMove: Option[Moves.Value] = None

  var opponentHistory = List[Moves.Value]()

  while (true) {
    val maybePreviousOpponentMove = parse(readLine)

    opponentHistory = opponentHistory ++ maybePreviousOpponentMove

    val myMove = (opponentHistory, maybeMyLastMove) match {
      case (h, Some(myLastMove)) if h.nonEmpty =>
        strategiesScores = strategiesScores.map { case (strategy, score) =>
          (
            strategy,
            score + strategy.getScore(h, myLastMove)
          )
        }
        strategiesScores.maxBy(_._2) match {
          case (strategy, score) =>
            System.err.println(
              s"Best strategy is ${strategy.getClass.getSimpleName} with score $score"
            )
            strategy.move(h)
        }
      case other =>
        System.err.println("First turn")
        randomIn(Moves.values)
    }

    maybeMyLastMove = Some(myMove)

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
      myLastMove: Moves.Value
  ): Double

  def move(previousOpponentMove: List[Moves.Value]): Moves.Value
}

class RandomStrategy extends Strategy {
  override def move(opponentHistory: List[Moves.Value]): Moves.Value =
    randomIn(Moves.values)

  override def getScore(
      opponentHistory: List[Moves.Value],
      myLastMove: Moves.Value
  ): Double = 0
}

class RockStrategy extends Strategy {
  override def move(opponentHistory: List[Moves.Value]): Moves.Value =
    Moves.ROCK

  override def getScore(
      opponentHistory: List[Moves.Value],
      myLastMove: Moves.Value
  ): Double =
    score(Moves.ROCK, opponentHistory.last)
}

class PaperStrategy extends Strategy {
  override def move(opponentHistory: List[Moves.Value]): Moves.Value =
    Moves.PAPER

  override def getScore(
      opponentHistory: List[Moves.Value],
      myLastMove: Moves.Value
  ): Double =
    score(Moves.PAPER, opponentHistory.last)
}

class ScissorsStrategy extends Strategy {
  override def move(opponentHistory: List[Moves.Value]): Moves.Value =
    Moves.SCISSORS

  override def getScore(
      opponentHistory: List[Moves.Value],
      myLastMove: Moves.Value
  ): Double =
    score(Moves.SCISSORS, opponentHistory.last)
}

class CopyOpponentStrategy extends Strategy {
  override def move(opponentHistory: List[Moves.Value]): Moves.Value =
    opponentHistory.last

  override def getScore(
      opponentHistory: List[Moves.Value],
      myLastMove: Moves.Value
  ): Double = score(
    myLastMove,
    opponentHistory.last
  )
}

class BeatLastOpponentStrategy extends Strategy {
  override def move(opponentHistory: List[Moves.Value]): Moves.Value =
    whoBeats(opponentHistory.last)

  override def getScore(
      opponentHistory: List[Moves.Value],
      myLastMove: Moves.Value
  ): Double = score(
    myLastMove,
    opponentHistory.last
  )
}

class BeatMostOpponentStrategyAll extends Strategy {

  override def move(opponentHistory: List[Moves.Value]): Moves.Value = {
    whoBeats(
      opponentHistory
        .groupBy(identity)
        .map { case (move, moves) =>
          (move, moves.length)
        }
        .maxBy(_._2)
        ._1
    )
  }

  override def getScore(
      opponentHistory: List[Moves.Value],
      myLastMove: Moves.Value
  ): Double = score(
    myLastMove,
    opponentHistory.last
  )
}

object Helpers {
  val random = new Random()

  def randomIn[T](values: Iterable[T]): T = {
    val index = random.nextInt(values.size)
    values.toList(index)
  }

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

  def whoBeats(move: Moves.Value): Moves.Value = {
    move match {
      case Moves.ROCK     => Moves.PAPER
      case Moves.PAPER    => Moves.SCISSORS
      case Moves.SCISSORS => Moves.ROCK
    }
  }
}
