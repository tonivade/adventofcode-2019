package adventofcode

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

object Day11 {
  import Day5.{State, Input}

  sealed trait Color {
    val id: Long
  }
  case object Black extends Color {
    override val id: Long = 0 
  }
  case object White extends Color {
    override val id: Long = 1 
  }

  sealed trait Direction {
    val left: Direction
    val right: Direction
  }
  case object Up extends Direction {
    override val left: Direction = Left
    override val right: Direction = Right
  }
  case object Down extends Direction {
    override val left: Direction = Right
    override val right: Direction = Left
  }
  case object Left extends Direction {
    override val left: Direction = Down
    override val right: Direction = Up
  }
  case object Right extends Direction {
    override val left: Direction = Up
    override val right: Direction = Down
  }

  type Position = (Int, Int)

  def move(position: Position, direction: Direction): Position = 
    (position, direction) match {
      case ((x, y), Up) => (x, y + 1)
      case ((x, y), Down) => (x, y - 1)
      case ((x, y), Left) => (x - 1, y)
      case ((x, y), Right) => (x + 1, y)
    }

  @tailrec
  def paint(hull: Map[Position, Color], position: Position, direction: Direction, state: State): Map[Position, Color] = {

    val current = hull.getOrElse(position, Black)

    val result = state.resume(Input(current.id))

    if (result.paused) {
      result.output match {
        case ListBuffer(0, 0) => {
          paint(hull + (position -> Black), move(position, direction.left), direction.left, result)
        }
        case ListBuffer(0, 1) => {
          paint(hull + (position -> Black), move(position, direction.right), direction.right, result)
        }
        case ListBuffer(1, 0) => {
          paint(hull + (position -> White), move(position, direction.left), direction.left, result)
        }
        case ListBuffer(1, 1) => {
          paint(hull + (position -> White), move(position, direction.right), direction.right, result)
        }
      }
    } else hull
  }
}

object Day11Part1 extends App {
  import Day5.{runProgram, loadProgram, Program, Input, State}
  import Day11._

  val program = loadProgram("input-day11.txt")

  val init = runProgram(program)

  val hull = paint(Map.empty[Position, Color], (0, 0), Up, init)

  println(hull.size)
}