import math._
import scala.util._
import scala.io.StdIn._

object Player extends App {

  val random = new Random()

  def randomIn(values: Moves.ValueSet) = {
    val index = random.nextInt(values.size)
    values.toList(index)
  }

  while (true) {
    val previousOpponentMove = readLine

    // get random move
    val move = randomIn(Moves.values)

    println(move)
  }
}

object Moves extends Enumeration {
  type Move = Value
  val ROCK, PAPER, SCISSORS = Value
}
