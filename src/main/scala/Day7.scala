package adventofcode

import scala.io.Source

object Day7 {
  import Day5.{runProgram, Input}

  def compute(program: Array[Int]): List[Int] =
    (0 to 4).permutations.map(sequence(program, _)).toList

  def sequence(program: Array[Int], sequence: IndexedSeq[Int]): Int = {
    val outA = runProgram(program, Input(sequence(0), 0))(0)
    val outB = runProgram(program, Input(sequence(1), outA))(0)
    val outC = runProgram(program, Input(sequence(2), outB))(0)
    val outD = runProgram(program, Input(sequence(3), outC))(0)
    runProgram(program, Input(sequence(4), outD))(0)
  }

  var program = Source.fromFile("src/main/resources/input-day7.txt").getLines
    .flatMap(_.split(',')).map(_.toInt).toArray

}

object Day7Part1 extends App {
  import Day7._

  println("Day7 Part2")

  println(compute(program).max)
}

object Day7Test extends App {
  import Day5.{runProgram, Input}
  import Day7._

  val program1 = Array(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
  assert(compute(program1).max == 43210)

  val program2 = Array(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
  assert(compute(program2).max == 54321)

  val program3 = Array(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
  assert(compute(program3).max == 65210)
}