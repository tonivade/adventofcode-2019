package adventofcode

import scala.io.Source
import scala.collection.mutable.ArraySeq

object Day7 {
  import Day5.{runProgram, runProgram2, loadProgram, Input}

  def compute(program: Array[Int]): List[Int] =
    (0 to 4).permutations.map(sequence(program, _)).toList

  def computeFeedback(program: Array[Int]): List[Int] = {
    val input = compute(program).max
    (5 to 9).permutations.map(feedback(program, _, input)).toList
  }

  def sequence(program: Array[Int], sequence: IndexedSeq[Int]): Int = {
    val outA = runProgram(program, Input(sequence(0), 0))(0)
    val outB = runProgram(program, Input(sequence(1), outA))(0)
    val outC = runProgram(program, Input(sequence(2), outB))(0)
    val outD = runProgram(program, Input(sequence(3), outC))(0)
    runProgram(program, Input(sequence(4), outD))(0)
  }

  def feedback(program: Array[Int], sequence: IndexedSeq[Int], input: Int): Int = {
    println(sequence)
    val p = program.to[ArraySeq]
    val outA = runProgram2(p, Input(sequence(0), input))(0)
    val outB = runProgram2(p, Input(sequence(1), outA))(0)
    val outC = runProgram2(p, Input(sequence(2), outB))(0)
    val outD = runProgram2(p, Input(sequence(3), outC))(0)
    runProgram2(p, Input(sequence(4), outD))(0)
  }

  val program = loadProgram("input-day7.txt")
}

object Day7Part1 extends App {
  import Day7._

  println("Day7 Part1")

  println(compute(program).max)
}

object Day7Part2 extends App {
  import Day7._

  println("Day7 Part2")

  println(computeFeedback(program))
}

object Day7Test extends App {
  import Day7._

  val program1 = Array(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
//  assert(compute(program1).max == 43210)

  val program2 = Array(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
//  assert(compute(program2).max == 54321)

  val program3 = Array(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
//  assert(compute(program3).max == 65210)

  val program4 = Array(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
  println(feedback(program4, IndexedSeq(9,8,7,6,5), 0))

  println("OK")
}