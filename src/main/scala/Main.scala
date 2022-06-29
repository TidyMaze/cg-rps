import math._
import scala.util._
import scala.io.StdIn._

object Player extends App {
  val allStrategies: Seq[Strategy] = Seq(new RandomStrategy)

  def parse(readLine: String): Moves.Value =
    readLine match {
      case "ROCK"     => Moves.ROCK
      case "PAPER"    => Moves.PAPER
      case "SCISSORS" => Moves.SCISSORS
      case _          => throw new IllegalArgumentException("Invalid move")
    }

  var strategiesScores = allStrategies.map(_ -> 0.toDouble).toMap

  while (true) {
    val previousOpponentMove = parse(readLine)

    strategiesScores = strategiesScores.map { case (strategy, score) =>
      (strategy, score + strategy.getScore(previousOpponentMove))
    }

    val move = strategiesScores.maxBy(_._2)._1.move(previousOpponentMove)

    println(move)
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

  val random = new Random()

  override def move(previousOpponentMove: Moves.Value): Moves.Value = {
    randomIn(Moves.values)

  }

  def randomIn(values: Moves.ValueSet) = {
    val index = random.nextInt(values.size)
    values.toList(index)
  }

  override def getScore(previousOpponentMove: Moves.Value): Double = 1.0 / 3.0
}
