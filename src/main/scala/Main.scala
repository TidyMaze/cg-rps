import Helpers.{random, randomIn, score}

import scala.util._
import scala.io.StdIn._

object Player extends App {
  val allStrategies: Seq[Strategy] = Seq(
    new RandomStrategy,
    new RockStrategy,
    new PaperStrategy,
    new ScissorsStrategy,
    new CopyOpponentStrategy,
    new BeatLastOpponentStrategy
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

  while (true) {
    val maybePreviousOpponentMove = parse(readLine)

    val myMove = (maybePreviousOpponentMove, maybeMyLastMove) match {
      case (Some(prevousOpponentMove), Some(myLastMove)) =>
        strategiesScores = strategiesScores.map { case (strategy, score) =>
          (strategy, score + strategy.getScore(prevousOpponentMove, myLastMove))
        }
        strategiesScores.maxBy(_._2) match {
          case (strategy, score) =>
            System.err.println(
              s"Best strategy is ${strategy.getClass.getSimpleName} with score $score"
            )
            strategy.move(prevousOpponentMove)
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
      previousOpponentMove: Moves.Value,
      myLastMove: Moves.Value
  ): Double

  def move(previousOpponentMove: Moves.Value): Moves.Value
}

class RandomStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value =
    randomIn(Moves.values)

  override def getScore(
      previousOpponentMove: Moves.Value,
      myLastMove: Moves.Value
  ): Double = 0
}

class RockStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value = Moves.ROCK

  override def getScore(
      previousOpponentMove: Moves.Value,
      myLastMove: Moves.Value
  ): Double =
    score(Moves.ROCK, previousOpponentMove)
}

class PaperStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value =
    Moves.PAPER

  override def getScore(
      previousOpponentMove: Moves.Value,
      myLastMove: Moves.Value
  ): Double =
    score(Moves.PAPER, previousOpponentMove)
}

class ScissorsStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value =
    Moves.SCISSORS

  override def getScore(
      previousOpponentMove: Moves.Value,
      myLastMove: Moves.Value
  ): Double =
    score(Moves.SCISSORS, previousOpponentMove)
}

class CopyOpponentStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value =
    previousOpponentMove

  override def getScore(
      previousOpponentMove: Moves.Value,
      myLastMove: Moves.Value
  ): Double = score(
    myLastMove,
    previousOpponentMove
  )
}

class BeatLastOpponentStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value =
    previousOpponentMove match {
      case Moves.ROCK     => Moves.PAPER
      case Moves.PAPER    => Moves.SCISSORS
      case Moves.SCISSORS => Moves.ROCK
    }

  override def getScore(
      previousOpponentMove: Moves.Value,
      myLastMove: Moves.Value
  ): Double = score(
    myLastMove,
    previousOpponentMove
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
}
