package adventofcode

import scala.io.Source
import scala.annotation.tailrec

object Day10 {

  sealed trait Space
  case object Empty extends Space
  case object Asteroid extends Space

  case class Position(x: Int, y: Int) {
    def move(motion: Motion): Position = 
      Position(x + motion._1, y + motion._2)

    lazy val id = (x * 100) + y
  }

  type Motion = (Int, Int)

  object MotionOrdering extends Ordering[Motion] {

    override def compare(a: Motion, b: Motion): Int = {
      val angleA = angle(a._1, a._2)
      val angleB = angle(b._1, b._2)
      angleA.compare(angleB)
    }

    def angle(x: Double, y: Double): Double = {
      val a = Math.atan2(y, x) * 180 / Math.PI match {
        case a if a > 0 => a
        case a if a < 0 => 360 + a
        case _ => 0
      }

      (90 + a) % 360
    }
  }

  case class Matrix(field: Vector[Vector[Space]]) {

    def get(position: Position): Option[Space] =
      if (field.isDefinedAt(position.y) && field(position.y).isDefinedAt(position.x))
        Some(field(position.y)(position.x))
      else
        None

    def map(f: (Position, Space) => Space): Matrix = {
      val m = field.zipWithIndex.map {
        case (row, y) => row.zipWithIndex.map {
          case (s, x) => f(Position(x, y), s)
        }
      }

      Matrix(m)
    }

    def visibleFrom(position: Position): (Position, Int) = {
      val count = motions.flatMap(search(position, _).toList).size
     
      (position, count)
    }

    def vaporizeFrom(position: Position): Seq[Position] =
      motions.flatMap(search(position, _).toList)

    def search(position: Position, motion: Motion): Option[Position] = {
      val next = position.move(motion)
      get(next) match {
        case Some(Asteroid) => Some(next)
        case Some(Empty) => search(next, motion)
        case None => None
      }
    }

    def asteroids: List[Position] =
      field.zipWithIndex.flatMap {
        case (row, y) => row.zipWithIndex.map {
          case (s, x) => (Position(x, y), s)
        }
      }.filter(_._2 == Asteroid).map(_._1).toList

    def motions: Seq[Motion] = 
      validMoves.flatMap {
        case (x, y) => List((x, y), (-x, y), (x, -y), (-x, -y))
      }.distinct.sorted(MotionOrdering)

    def validMoves: Seq[Motion] = {
      val all = for {
        y <- (0 until field.size)
        x <- (0 until field(0).size)
      } yield (x, y)

      all.filter(valid)
    }

    def valid(motion: (Int, Int)): Boolean = {
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

    def location: Position = findAll.maxBy(_._2)._1
    def maxVisibility: Int = findAll.maxBy(_._2)._2
  }

  def parseLine(line: String): Vector[Space] =
    line.map {
      case '#' => Asteroid
      case '.' => Empty
    }.toVector

  def parseMatrix(input: String): Matrix = 
    Matrix(input.linesIterator.map(parseLine).toVector)

  def vaporize(m: Matrix, p: Position): Seq[Position] = {
    val vaporized = m.vaporizeFrom(p)

    val m1 = m.map {
      case (p, Asteroid) => if (vaporized.contains(p)) Empty else Asteroid
      case (p, Empty) => Empty
    }

    if (m1.asteroids.size == 1)
      vaporized
    else
      vaporized ++ vaporize(m1, p)
  }
}

object Day10Part1 extends App {
  import Day10._

  val matrix = parseMatrix(Source.fromResource("input-day10.txt").mkString)

  println(matrix.maxVisibility)
}

object Day10Part2 extends App {
  import Day10._

  val matrix = parseMatrix(Source.fromResource("input-day10.txt").mkString)

  val position = matrix.location

  println(vaporize(matrix, position)(200 - 1).id)
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
  assert(m5.maxVisibility == 210)

  val input6 = """.#....#####...#..
                 |##...##.#####..##
                 |##...#...#.#####.
                 |..#.....#...###..
                 |..#.#.....#....##""".stripMargin
  val m6 = parseMatrix(input6)
  vaporize(m6, Position(8, 3)).foreach(println)

  println("OK")
}