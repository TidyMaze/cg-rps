import Helpers.{random, randomIn}

import scala.util._
import scala.io.StdIn._

object Player extends App {
  val allStrategies: Seq[Strategy] = Seq(
    new RandomStrategy,
    new RockStrategy,
    new PaperStrategy,
    new ScissorsStrategy
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

  while (true) {
    val previousOpponentMove = parse(readLine)

    val myMove = previousOpponentMove match {
      case Some(move) =>
        strategiesScores = strategiesScores.map { case (strategy, score) =>
          (strategy, score + strategy.getScore(move))
        }
        strategiesScores.maxBy(_._2) match {
          case (strategy, score) =>
            System.err.println(
              s"Best strategy is ${strategy.getClass.getSimpleName} with score $score"
            )
            strategy.move(move)
        }
      case None =>
        System.err.println("First turn")
        randomIn(Moves.values)
    }

    println(myMove.toString)
  }
}

object Moves extends Enumeration {
  type Move = Value
  val ROCK, PAPER, SCISSORS = Value
}

trait Strategy {
  def getScore(previousOpponentMove: Moves.Value): Double

  def move(previousOpponentMove: Moves.Value): Moves.Value
}

class RandomStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value =
    randomIn(Moves.values)

  override def getScore(previousOpponentMove: Moves.Value): Double = 1.0 / 3.0
}

class RockStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value = Moves.ROCK

  override def getScore(previousOpponentMove: Moves.Value): Double =
    if (previousOpponentMove == Moves.PAPER) 1.0 else 0.0
}

class PaperStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value =
    Moves.PAPER

  override def getScore(previousOpponentMove: Moves.Value): Double =
    if (previousOpponentMove == Moves.ROCK) 1.0 else 0.0
}

class ScissorsStrategy extends Strategy {
  override def move(previousOpponentMove: Moves.Value): Moves.Value =
    Moves.SCISSORS

  override def getScore(previousOpponentMove: Moves.Value): Double =
    if (previousOpponentMove == Moves.PAPER) 1.0 else 0.0
}

object Helpers {
  val random = new Random()

  def randomIn[T](values: Iterable[T]): T = {
    val index = random.nextInt(values.size)
    values.toList(index)
  }
}
