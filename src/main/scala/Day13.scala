package adventofcode

import scala.io.Source

object Day13 extends App {
  import Day5.{runProgram, loadProgram}

  case class Tile(position: (Int, Int), id: Id) {
    def toChar: Char = {
      id match {
        case Ball => 'O'
        case Block => 'X'
        case Empty => ' '
        case HorizontalPaddle => '='
        case Wall => '$'
      }
    }
  }

  sealed trait Id
  case object Empty extends Id
  case object Wall extends Id
  case object Block extends Id
  case object HorizontalPaddle extends Id
  case object Ball extends Id

  def parseTile(input: Seq[Long]): Tile =
    input match {
      case Seq(x, y, 0) => Tile((x.toInt, y.toInt), Empty)
      case Seq(x, y, 1) => Tile((x.toInt, y.toInt), Wall)
      case Seq(x, y, 2) => Tile((x.toInt, y.toInt), Block)
      case Seq(x, y, 3) => Tile((x.toInt, y.toInt), HorizontalPaddle)
      case Seq(x, y, 4) => Tile((x.toInt, y.toInt), Ball)
    }
  
  def paintGrid(grid: Seq[Tile]): String = {
    val width = grid.map(_.position._1).max.toInt
    val height = grid.map(_.position._2).max.toInt

    val positions = for {
      y <- 0 to height
      x <- 0 to width
    } yield (x, y)

    val tiles = grid.map(t => (t.position, t.toChar)).toMap

    positions.grouped(width + 1).map(x => x.map(z => tiles(z)).mkString).mkString("\n")
  }
  
  val program = loadProgram("input-day13.txt")

  val result = runProgram(program)

  val grid = result.output.grouped(3).map(parseTile).toList

  println(grid.count(_.id == Block))

  println(paintGrid(grid))
}