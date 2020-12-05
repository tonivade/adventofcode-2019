package adventofcode

import scala.io.Source
import scala.collection.immutable.Stream.Cons

case class Point(x: Int, y: Int) {
  val distance = Math.abs(x) + Math.abs(y)
}

case class Line(p1: Point, p2: Point) {

  val maxX = Math.max(p1.x, p2.x)
  val minX = Math.min(p1.x, p2.x)
  val maxY = Math.max(p1.y, p2.y)
  val minY = Math.min(p1.y, p2.y)

  def intersection(other: Line): Option[Point] = {
    val s1 = this.p1
    val s2 = this.p2
    val d1 = other.p1
    val d2 = other.p2

    val a1 = s2.y - s1.y
    val b1 = s1.x - s2.x
    val c1 = a1 * s1.x + b1 * s1.y

    val a2 = d2.y - d1.y
    val b2 = d1.x - d2.x
    val c2 = a2 * d1.x + b2 * d1.y

    val delta = a1 * b2 - a2 * b1

    if (delta != 0)
      Some(Point((b2 * c1 - b1 * c2) / delta, (a1 * c2 - a2 * c1) / delta))
        .filter(p => this.contains(p) && other.contains(p))
    else 
      None
  }

  def contains(p: Point): Boolean = 
    p.x >= minX && p.x <= maxX && p.y >= minY && p.y <= maxY

  val length: Int = Math.max(Math.abs(maxX - minX), Math.abs(maxY - minY))
}

object Day3 {

  def parse(line: String): List[(Char, Int)] =
    line.split(',').map(x => (x(0), x.drop(1).toInt)).toList

  val input = Source.fromFile("src/main/resources/input-day3.txt").getLines.toArray
  
  val input1 = input(0)
  val input2 = input(1)

  def lines(input: List[(Char, Int)]): List[Line] = {
    input.foldLeft(((0, 0), List[Line]()))((state, move) => {
      val (current, list) = state
      val next = move._1 match {
        case 'U' => (current._1, current._2 + move._2)
        case 'D' => (current._1, current._2 - move._2)
        case 'L' => (current._1 - move._2, current._2)
        case 'R' => (current._1 + move._2, current._2)
      }
      (next, list :+ Line(Point(current._1, current._2), Point(next._1, next._2)))
    })._2
  }

  def intersections(input1: List[Line], input2: List[Line]): List[Point] =
    input1.flatMap(a => input2.flatMap(b => a.intersection(b).toList))

  def distance(input1: String, input2: String): Int = {
    val lines1 = lines(parse(input1))
    val lines2 = lines(parse(input2))

    intersections(lines1, lines2).map(_.distance).filter(_ > 0).min
  }

  def stepsInLine(line: List[Line], p: Point): Int = {
    val path = line.takeWhile(!_.contains(p))

    val pathWithLast = path.lastOption match {
      case Some(last) => path :+ Line(last.p2, p)
      case None => path
    }

    pathWithLast match {
      case Nil => 0
      case xs => xs.map(_.length).sum
    }
  }

  def steps(input1: String, input2: String): Int = {
    val lines1 = lines(parse(input1))
    val lines2 = lines(parse(input2))

    intersections(lines1, lines2)
      .map(p => stepsInLine(lines1, p) + stepsInLine(lines2, p)).filter(_ > 0).min
  }
}

object Day3Part1 extends App {
  import Day3._

  println("Day3 Part1")

  println(distance(input1, input2))
}

object Day3Part2 extends App {
  import Day3._
  
  println("Day3 Part2")

  println(steps(input1, input2))
}

object Day3Test extends App {
  import Day3._

  assert(distance("R8,U5,L5,D3", "U7,R6,D4,L4") == 6)
  assert(distance("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83") == 159)
  assert(distance("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") == 135)
  
  assert(steps("R8,U5,L5,D3", "U7,R6,D4,L4") == 30)
  assert(steps("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83") == 610)
  assert(steps("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") == 410)
}