package adventofcode

import scala.io.Source
import scala.io.StdIn
import scala.annotation.tailrec

object Day13 {
  import Day5.{loadProgram, Input, Output, State}

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

  type Score = Int

  sealed trait Id
  case object Empty extends Id
  case object Wall extends Id
  case object Block extends Id
  case object HorizontalPaddle extends Id
  case object Ball extends Id

  def parseTile(input: Seq[Long]): Either[Score, Tile] =
    input match {
      case Seq(-1, 0, score) => Left(score.toInt)
      case Seq(x, y, 0) => Right(Tile((x.toInt, y.toInt), Empty))
      case Seq(x, y, 1) => Right(Tile((x.toInt, y.toInt), Wall))
      case Seq(x, y, 2) => Right(Tile((x.toInt, y.toInt), Block))
      case Seq(x, y, 3) => Right(Tile((x.toInt, y.toInt), HorizontalPaddle))
      case Seq(x, y, 4) => Right(Tile((x.toInt, y.toInt), Ball))
    }

  def parseOutput(output: Output): (Score, Seq[Tile]) =
   output.grouped(3).map(parseTile).foldLeft((0, List.empty[Tile])) {
     case ((current, list), Left(score)) => (score, list)
     case ((current, list), Right(tile)) => (current, list :+ tile)
   }
  
  def paintGrid(output: (Score, Seq[Tile])): String = {
    output match {
      case (score, grid) => {
        val width = grid.map(_.position._1).max.toInt
        val height = grid.map(_.position._2).max.toInt

        val positions = for {
          y <- 0 to height
          x <- 0 to width
        } yield (x, y)

        val tiles = grid.map(t => (t.position, t.toChar)).toMap

        positions.grouped(width + 1)
          .map(x => x.map(z => tiles(z)).mkString).mkString("\n") + 
            s"\nscore: $score, blocks: ${grid.count(_.id == Block)}"
      }
    }
  }

  def updateGrid(grid: (Score, Seq[Tile]), update: (Score, Seq[Tile])): (Score, Seq[Tile]) = {

    val map = update._2.map(x => (x.position, x.id)).toMap

    val newGrid = grid._2.map {
      case Tile(p, t) =>
        if (map.contains(p)) {
          Tile(p, map(p))
        } else Tile(p, t)
    }

    (Math.max(grid._1, update._1), newGrid)
  }

  @tailrec
  def runGame(state: State, current: (Score, Seq[Tile])): State = {
    val line = StdIn.readLine()

    val move = line match {
      case "a" => -1
      case "f" => 1
      case _ => 0
    }

    val next = state.resume(Input(move))
    val output = parseOutput(next.output)
    val grid = updateGrid(current, output)

    println(paintGrid(grid))
    
    if (next.paused)
      runGame(next, grid)
    else 
      next
  }
  
  val program = loadProgram("input-day13.txt")
}

object Day13Part1 extends App {
  import Day5.runProgram
  import Day13._

  val result = runProgram(program)

  val grid = parseOutput(result.output)

  println(grid._2.count(_.id == Block))

  println(paintGrid(grid))
}

object Day13Part2 extends App {
  import Day5.{runProgram, fixProgram, Input}
  import Day13._

  val fixed = fixProgram(program)(0, 2)

  val result = runProgram(fixed)
  val grid = parseOutput(result.output)

  println(paintGrid(grid))

  println(runGame(result, grid).stopped)
}