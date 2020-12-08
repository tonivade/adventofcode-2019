package adventofcode

import scala.io.Source

object Day8 {
  val layerSize = 25 * 6
  val input = Source.fromFile("src/main/resources/input-day8.txt").mkString.dropRight(1)

  val layers = input.grouped(layerSize)

  def applyLayer(result: String, layer: String): String =
    result.zip(layer).map(t => if (t._1 == '2') t._2 else t._1).mkString

  def paint(layers: Iterator[String], x: Int, y: Int): String =
    layers.foldLeft("2".repeat(x * y))(applyLayer).grouped(x).mkString("\n")
}

object Day8Part1 extends App {
  import Day8._

  println("Day8 Part1")

  val counts = layers.map(l => (l.count(_ == '0'), l.count(_ == '1'), l.count(_ == '2')))
  val min = counts.toList.sortBy(_._1).min
  println(min._2 * min._3)
}

object Day8Part2 extends App {
  import Day8._
  
  println("Day8 Part2")

  println(paint(layers, 25, 6).replace('0', ' ').replace('1', '#'))
}

object Day8Test extends App {
  import Day8._

  val test = "0222112222120000".grouped(2*2)
  
  assert(paint(test, 2, 2) == "01\n10")

  println("OK")
}