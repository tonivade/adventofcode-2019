package adventofcode

import scala.io.Source
import scala.annotation.tailrec

object Day1 {

  def fuel(mass: Int): Int = 
    Math.round(mass / 3) - 2

  def fuelRecursive(mass: Int): Int = {
    val f = fuel(mass)
    if (f > 0) 
      f + fuelRecursive(f)
    else
      0
  }

  val lines = Source.fromFile("src/main/resources/input-day1.txt").getLines().map(_.toInt)
}

object Day1Part1 extends App {
  import Day1._

  println("Day1 Part1")

  println(lines.map(fuel).sum)
}

object Day1Part2 extends App {
  import Day1._

  println("Day1 Part2")

  println(lines.map(fuelRecursive).sum)
}

object Day1Test extends App {
  import Day1._

  assert(fuel(12) == 2)
  assert(fuelRecursive(12) == 2)
  assert(fuel(14) == 2)
  assert(fuelRecursive(14) == 2)
  assert(fuel(1969) == 654)
  assert(fuelRecursive(1969) == 966)
  assert(fuel(100756) == 33583)
  assert(fuelRecursive(100756) == 50346)
}