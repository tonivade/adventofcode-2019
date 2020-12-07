package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day6 {

  type Orbit = (String, String)

  def parseOrbit(line: String): Orbit = {
    val split = line.split(')')

    (split(1), split(0))
  }

  def count(name: String, orbits: Map[String, String]): Int =
    orbits.get(name).fold(0)(1 + count(_, orbits))
  
  def search(name: String, orbits: Map[String, String]): List[String] =
    orbits.get(name).fold(name :: Nil)(name :: search(_, orbits))

  def total(orbits: Map[String, String]): Int =
    (orbits.keys ++ orbits.values).toSeq.map(count(_, orbits)).sum

  def path(source: String, target: String, orbits: Map[String, String]): List[String] = {
    val pathA = search(source, orbits)
    val pathB = search(target, orbits)

    val x = pathA.find(pathB.contains(_)).orNull

    val atox = pathA.takeWhile(_ != x).drop(1)
    val btox = pathB.takeWhile(_ != x).drop(1)

    (atox :+ x) ++ btox.reverse
  }

  def jumps(source: String, target: String, orbits: Map[String, String]): Int =
    path(source, target, orbits).size - 1
 
  val orbits = Source.fromFile("src/main/resources/input-day6.txt").getLines().map(parseOrbit).toMap
}

object Day6Part1 extends App {
  import Day6._

  println("Day6 Part1")

  println(total(orbits))
}

object Day6Part2 extends App {
  import Day6._

  println("Day6 Part2")

  println(jumps("YOU", "SAN", orbits))
}

object Day6Test extends App {
  import Day6._

  val example = """COM)B
                  |B)C
                  |C)D
                  |D)E
                  |E)F
                  |B)G
                  |G)H
                  |D)I
                  |E)J
                  |J)K
                  |K)L""".stripMargin

  val orbits1 = example.split('\n').map(parseOrbit).toMap

  assert(count("COM", orbits1) == 0)
  assert(count("D", orbits1) == 3)
  assert(count("L", orbits1) == 7)
  assert(total(orbits1) == 42)

  val example2 = """COM)B
                    |B)C
                    |C)D
                    |D)E
                    |E)F
                    |B)G
                    |G)H
                    |D)I
                    |E)J
                    |J)K
                    |K)L
                    |K)YOU
                    |I)SAN""".stripMargin
  
  val orbits2 = example2.split('\n').map(parseOrbit).toMap

  assert(jumps("YOU", "SAN", orbits2) == 4)
}