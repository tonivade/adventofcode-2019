package adventofcode

import scala.io.Source

object Day10 {

  sealed trait Space
  case object Empty extends Space
  case object Asteroid extends Space

  case class Position(x: Int, y: Int) {
    def move(motion: Motion): Position = 
      Position(x + motion._1, y + motion._2)
  }

  type Motion = (Int, Int)

  case class Matrix(field: Vector[Vector[Space]]) {

    def all: Seq[Position] = {
      for {
        y <- 0 until field.size
        x <- 0 until field(0).size
      } yield Position(x, y)
    }

    def get(position: Position): Option[Space] =
      if (field.isDefinedAt(position.y) && field(position.y).isDefinedAt(position.x))
        Some(field(position.y)(position.x))
      else
        None

    def visibleFrom(position: Position): (Position, Int) = {
      val count = motions.flatMap(search(position, _).toList).count(_ == Asteroid)
     
      (position, count)
    }

    def search(position: Position, motion: Motion): Option[Space] =
      get(position.move(motion)) match {
        case Some(Asteroid) => Some(Asteroid)
        case Some(Empty) => search(position.move(motion), motion)
        case None => None
      }

    def visitAll(position: Position): Seq[Position] = {
      motions.flatMap(visit(position, _))
    }

    def notVisited(position: Position): Set[Position] =
      all.toSet -- visitAll(Position(0, 0))

    def visit(position: Position, motion: Motion): List[Position] = {
      val next = position.move(motion)

      get(next) match {
        case Some(_) => next :: visit(next, motion)
        case None => Nil
      }
    }

    def asteroids: List[Position] =
      field.zipWithIndex.flatMap {
        case (row, y) => row.zipWithIndex.map {
          case (s, x) => (Position(x, y), s)
        }
      }.filter(_._2 == Asteroid).map(_._1).toList

    def motions: Seq[Motion] = {
      val all = for {
        y <- (0 until field.size)
        x <- (0 until field(0).size)
      } yield (x, y)

      val validMoves = all.filter(valid)

      validMoves.flatMap {
        case (x, y) => List((x, y), (-x, y), (x, -y), (-x, -y))
      }.toSet.toList.sorted
    }

    def valid(motion: (Int, Int)): Boolean  = {
      motion match {
        case (0, 0) => false
        // down
        case (0, y) if (y > 1) => false
        // right
        case (x, 0) if (x > 1) => false 
        // down + right
        case (x, y) if (x == y && x > 1 && y > 1) => false
        case (x, y) if (gcd(x, y) == 1) => true
        case _ => false
      }
    }

    def gcd(a: Int, b: Int): Int = BigInt(a).gcd(b).toInt

    def findAll: List[(Position, Int)] = asteroids.map(visibleFrom(_))

    def maxVisibility: Int = findAll.maxBy(_._2)._2
  }

  def parseLine(line: String): Vector[Space] =
    line.map {
      case '#' => Asteroid
      case '.' => Empty
    }.toVector

  def parseMatrix(input: String): Matrix = 
    Matrix(input.linesIterator.map(parseLine).toVector)
}

object Day10Part1 extends App {
  import Day10._

  val matrix = parseMatrix(Source.fromResource("input-day10.txt").mkString)

  println(matrix.maxVisibility)
}

object Day10Test extends App {
  import Day10._

  val input1 = """.#..#
                 |.....
                 |#####
                 |....#
                 |...##""".stripMargin
  val m1 = parseMatrix(input1)
  /*
    .7..7
    .....
    67775
    ....7
    ...87
   */
  m1.findAll.foreach(println)
  assert(m1.maxVisibility == 8)

  val input2 = """......#.#.
                 |#..#.#....
                 |..#######.
                 |.#.#.###..
                 |.#..#.....
                 |..#....#.#
                 |#..#....#.
                 |.##.#..###
                 |##...#..#.
                 |.#....####""".stripMargin
  val m2 = parseMatrix(input2)
  m2.findAll.foreach(println)
  assert(m2.maxVisibility == 33)

  val input3 = """#.#...#.#.
                 |.###....#.
                 |.#....#...
                 |##.#.#.#.#
                 |....#.#.#.
                 |.##..###.#
                 |..#...##..
                 |..##....##
                 |......#...
                 |.####.###.""".stripMargin
  val m3 = parseMatrix(input3)
  m3.findAll.foreach(println)
  assert(m3.maxVisibility == 35)

  val input4 = """.#..#..###
                 |####.###.#
                 |....###.#.
                 |..###.##.#
                 |##.##.#.#.
                 |....###..#
                 |..#.#..#.#
                 |#..#.#.###
                 |.##...##.#
                 |.....#.#..""".stripMargin
  val m4 = parseMatrix(input4)
  m4.findAll.foreach(println)
  assert(m4.maxVisibility == 41)

  val input5 = """.#..##.###...#######
                 |##.############..##.
                 |.#.######.########.#
                 |.###.#######.####.#.
                 |#####.##.#.##.###.##
                 |..#####..#.#########
                 |####################
                 |#.####....###.#.#.##
                 |##.#################
                 |#####.##.###..####..
                 |..######..##.#######
                 |####.##.####...##..#
                 |.#####..#.######.###
                 |##...#.##########...
                 |#.##########.#######
                 |.####.#.###.###.#.##
                 |....##.##.###..#####
                 |.#.#.###########.###
                 |#.#.#.#####.####.###
                 |###.##.####.##.#..##""".stripMargin
  val m5 = parseMatrix(input5)
  m5.findAll.foreach(println)
  assert(m5.maxVisibility == 210)

  println("OK")
}