package adventofcode

import scala.annotation.tailrec
import scala.io.Source

object Day6 {

  type Orbit = (String, String)

  def parseOrbit(line: String): Orbit = {
    val split = line.split(')')

    (split(1), split(0))
  }

  def search(name: String, orbits: Map[String, String]): Int =
    orbits.get(name).fold(0)(1 + search(_, orbits))

  def total(orbits: Map[String, String]): Int =
    (orbits.keys ++ orbits.values).toSeq.map(search(_, orbits)).sum
}

object Day6Part1 extends App {
  import Day6._

  val orbits = Source.fromFile("src/main/resources/input-day6.txt").getLines().map(parseOrbit).toMap

  println(total(orbits))
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

  val orbits = example.split('\n').map(parseOrbit).toMap

  assert(search("COM", orbits) == 0)
  assert(search("D", orbits) == 3)
  assert(search("L", orbits) == 7)
  assert(total(orbits) == 42)
  
}