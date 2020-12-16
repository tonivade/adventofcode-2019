package adventofcode

import scala.io.Source
import scala.annotation.tailrec

object Day12 {

  type Speed = (Int, Int, Int)
  
  object Speed {
    val zero = (0, 0, 0)
  }

  type Position = (Int, Int, Int)

  case class Moon(name: String, position: Position, speed: Speed = Speed.zero) {

    def throttle(delta: Speed): Moon =
      (speed, delta) match {
        case ((x, y, z), (dx, dy, dz)) => Moon(name, position, (x + dx, y + dy, z + dz))
      }

    def update: Moon =
      (speed, position) match {
        case ((sx, sy, sz), (x, y, z)) => Moon(name, (x + sx, y + sy, z + sz), speed)
      }

    def energy: Int = {
      val pot = Math.abs(position._1) + Math.abs(position._2) + Math.abs(position._3)
      val kin = Math.abs(speed._1) + Math.abs(speed._2) + Math.abs(speed._3)
      pot * kin
    }

    def eqX(other: Moon): Boolean = this.position._1 == other.position._1 && this.speed._1 == other.speed._1
    def eqY(other: Moon): Boolean = this.position._2 == other.position._2 && this.speed._2 == other.speed._2
    def eqZ(other: Moon): Boolean = this.position._3 == other.position._3 && this.speed._3 == other.speed._3
  }

  def delta(a: Int, b: Int): (Int, Int) = 
    if (a < b)
      (1, -1)
    else if (a > b)
      (-1, 1)
    else
      (0, 0)

  def gravity(moons: Seq[Moon]): Seq[Moon] = {
    val combinations = moons.map(_.name).combinations(2).map { case Seq(m1, m2) => (m1, m2) }

    val initial = moons.map(m => (m.name, m)).toMap

    val result = combinations.foldLeft(initial) {
      case (state, (name1, name2)) => {
        val m1 = state(name1)
        val m2 = state(name2)

        (m1, m2) match {
          case (Moon(n1, (x1, y1, z1), s1), Moon(n2, (x2, y2, z2), s2)) => {
            val (dx1, dx2) = delta(x1, x2)
            val (dy1, dy2) = delta(y1, y2)
            val (dz1, dz2) = delta(z1, z2)

            state + (m1.name -> m1.throttle(dx1, dy1, dz1)) + (m2.name -> m2.throttle(dx2, dy2, dz2))
          }
          case _ => throw new RuntimeException(s"m1=$m1,m2=$m2")
        }
      }
    }

    result.values.toList
  }

  def velocity(moons: Seq[Moon]): Seq[Moon] = moons.map(_.update).toList

  def step(moons: Seq[Moon]): Seq[Moon] = velocity(gravity(moons))

  def run(limit: Int)(moons: Seq[Moon]): Seq[Moon] =
    if (limit > 0) 
      run(limit - 1)(step(moons))
    else 
      moons

  def simulate(initial: Seq[Moon], check: (Moon, Moon) => Boolean): Long = {

    @tailrec
    def loop(initial: Seq[Moon], current: Seq[Moon], counter: Long): Long = {
      if (counter % 1000 == 0) println(counter)

      val next = step(current)

      if (initial.zip(next).forall { case (m1, m2) => check(m1, m2)})
        counter
      else
        loop(initial, next, counter + 1)
    }

    loop(initial, initial, 1)
  }

  def total(moons: Seq[Moon]): Int = moons.map(_.energy).sum

  def parseMoon(pos: Int, line: String): Moon = {
    val regex = """<x=(\-?\d+), y=(\-?\d+), z=(\-?\d+)>""".r

    line match {
      case regex(x, y, z) => Moon(s"m$pos", (x.toInt, y.toInt, z.toInt))
    }
  }

  def parseInput(input: String): Seq[Moon] =
    input.linesIterator.zipWithIndex.map { case (line, pos) => parseMoon(pos, line) }.toSeq

  def lcm(a: BigInt, b: BigInt): BigInt = (a * b).abs / (a gcd b)

  val input = Source.fromResource("input-day12.txt").mkString
  val moons = parseInput(input)
}

object Day12Part1 extends App {
  import Day12._

  println(total(run(1000)(moons)))
}

object Day12Part2 extends App {
  import Day12._

  val x = simulate(moons, _ eqX _)
  val y = simulate(moons, _ eqY _)
  val z = simulate(moons, _ eqZ _)

  println(lcm(x, lcm(y, z)))
}

object Day12Test extends App {
  import Day12._

  val m1 = Moon("m1", (-1, 0, 2))
  val m2 = Moon("m2", (2, -10, -7))
  val m3 = Moon("m3", (4, -8, 8))
  val m4 = Moon("m4", (3, 5, -1))

  val result = run(10)(Seq(m1, m2, m3, m4))

  /*
   * pos=<x= 2, y= 1, z=-3>, vel=<x=-3, y=-2, z= 1>
   * pos=<x= 1, y=-8, z= 0>, vel=<x=-1, y= 1, z= 3>
   * pos=<x= 3, y=-6, z= 1>, vel=<x= 3, y= 2, z=-3>
   * pos=<x= 2, y= 0, z= 4>, vel=<x= 1, y=-1, z=-1>
   */
  result.foreach(println)
  
  assert(total(result) == 179)
  assert(simulate(Seq(m1, m2, m3, m4), _ == _) == 2772)
}