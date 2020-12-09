package adventofcode

import scala.io.Source
import scala.annotation.tailrec

object Day7 {
  import Day5.{runProgram, loadProgram, Input, State, Program}

  def compute(program: Program): List[Long] =
    (0 to 4).permutations.map(sequence(program, _)).toList

  def computeFeedback(program: Program): List[Long] = 
    (5 to 9).permutations.map(feedback(program, _)).toList

  def sequence(program: Program, sequence: IndexedSeq[Int]): Long = {
    val outA = runProgram(program, Input(sequence(0), 0))
    val outB = runProgram(program, Input(sequence(1), outA.output(0)))
    val outC = runProgram(program, Input(sequence(2), outB.output(0)))
    val outD = runProgram(program, Input(sequence(3), outC.output(0)))
    val outE = runProgram(program, Input(sequence(4), outD.output(0)))
    outE.output(0)
  }

  def feedback(program: Program, sequence: IndexedSeq[Int]): Long = {
    val outA = runProgram(program, Input(sequence(0), 0))
    val outB = runProgram(program, Input(sequence(1), outA.output(0)))
    val outC = runProgram(program, Input(sequence(2), outB.output(0)))
    val outD = runProgram(program, Input(sequence(3), outC.output(0)))
    val outE = runProgram(program, Input(sequence(4), outD.output(0)))

    @tailrec
    def loop(stateA: State, stateB: State, stateC: State, stateD: State, stateE: State): Long = {
      val newA = stateA.resume(stateE.output)
      val newB = stateB.resume(newA.output)
      val newC = stateC.resume(newB.output)
      val newD = stateD.resume(newC.output)
      val newE = stateE.resume(newD.output)

      if (newE.stopped)
        newE.output(0)
      else 
        loop(newA, newB, newC, newD, newE)
    }

    loop(outA, outB, outC, outD, outE)
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

  println(computeFeedback(program).max)
}

object Day7Test extends App {
  import Day5.Program
  import Day7._

  val program1 = Program(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
  assert(compute(program1).max == 43210)

  val program2 = Program(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
  assert(compute(program2).max == 54321)

  val program3 = Program(3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
  assert(compute(program3).max == 65210)

  val program4 = Program(3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5)
  assert(computeFeedback(program4).max == 139629729)

  val program5 = Program(3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10)
  assert(computeFeedback(program5).max == 18216)

  println("OK")
}