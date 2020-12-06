package adventofcode

import scala.collection.mutable.ArraySeq
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.ListBuffer
import adventofcode.Day5.Computer
import scala.util.hashing.Hashing.Default

object Day5 {

  type Program = ArraySeq[Int]
  type Input = ListBuffer[Int]
  type Output = ListBuffer[Int]
  
  val Program = ArraySeq
  val Input = ListBuffer
  val Output = ListBuffer

  val POSITION_MODE = 0
  val IMMEDIATE_MODE = 1

  val OP_SUM = 1
  val OP_MULTIPLY = 2
  val OP_INPUT = 3
  val OP_OUTPUT = 4
  val OP_JUMP_IF_TRUE = 5
  val OP_JUMP_IF_FALSE = 6
  val OP_LESS_THAN = 7
  val OP_EQUALS = 8
  val OP_HALT = 99

  case class Computer(program: Program, input: Input = ListBuffer.empty, output: Output = ListBuffer.empty) {

    def read(): Int = input.remove(0)

    def write(value: Int): Unit = output.append(value)

    def param(mode: Int, current: Int): Int = 
      mode match {
        case POSITION_MODE => program(program(current))
        case IMMEDIATE_MODE => program(current)
      }

    def operation(mode: Int, current: Int, operation: (Int, Int) => Int) {
      val a = param(mode1(mode), current + 1)
      val b = param(mode2(mode), current + 2)
      update(program(current + 3), operation(a, b))
    }

    def update(current: Int, value: Int) = 
      program.update(current, value)

    def mode1(mode: Int) = mode % 10
    def mode2(mode: Int) = (mode / 10) % 10

    @tailrec
    final def run(current: Int = 0): Output = {
//      debug(current)
      val command = program(current)
      val mode = command / 100
      command % 100 match {
        case OP_SUM => {
          operation(mode, current, (a, b) => a + b)
          run(current + 4)
        }
        case OP_MULTIPLY => {
          operation(mode, current, (a, b) => a * b)
          run(current + 4)
        }
        case OP_INPUT => {
          val a = read()
          val p = program(current + 1)
          update(p, a)
          run(current + 2)
        }
        case OP_OUTPUT => {
          val a = param(mode1(mode), current + 1)
          write(a)
          run(current + 2)
        }
        case OP_JUMP_IF_TRUE => {
          val a = param(mode1(mode), current + 1)
          val b = param(mode2(mode), current + 2)
          run(if (a != 0) b else current + 3)
        }
        case OP_JUMP_IF_FALSE => {
          val a = param(mode1(mode), current + 1)
          val b = param(mode2(mode), current + 2)
          run(if (a == 0) b else current + 3)
        }
        case OP_LESS_THAN => {
          val a = param(mode1(mode), current + 1)
          val b = param(mode2(mode), current + 2)
          val c = program(current + 3)
          if (a < b) 
            update(c, 1)
          else
            update(c, 0)
          run(current + 4)
        }
        case OP_EQUALS => {
          val a = param(mode1(mode), current + 1)
          val b = param(mode2(mode), current + 2)
          val c = program(current + 3)
          if (a == b) 
            update(c, 1)
          else
            update(c, 0)
          run(current + 4)
        }
        case OP_HALT => output
      }
    }

    def debug(current: Int = 0) {
      println(s"computer pointer: $current")
      println(program)
      println(input)
      println(output)
      println()
    }
  }

  var program = Source.fromFile("src/main/resources/input-day5.txt").getLines
    .flatMap(_.split(',')).map(_.toInt).to[ArraySeq]

  def runProgram(program: Program, input: Input = Input.empty): Output = 
    Computer(program, input).run()
}

object Day5Part1 extends App {
  import Day5._

  println("Day5 Part1")

  println(runProgram(program, Input(1)))
}

object Day5Part2 extends App {
  import Day5._

  println("Day5 Part2")

  println(runProgram(program, Input(5)))
}

object Day5Test extends App {
  import Day5._
  
  assert(runProgram(Program(3, 0, 4, 0, 99), Input(1))(0) == 1)

  val c2 = Computer(Program(1002, 4, 3, 4, 33))
  c2.run()
  assert(c2.program(4) == 99)

  val c3 = Computer(Program(1101, 100, -1, 4, 0))
  c3.run()
  assert(c3.program(4) == 99)

  assert(runProgram(Program(3,9,8,9,10,9,4,9,99,-1,8), Input(8))(0) == 1)
  assert(runProgram(Program(3,9,8,9,10,9,4,9,99,-1,8), Input(9))(0) == 0)

  assert(runProgram(Program(3,9,7,9,10,9,4,9,99,-1,8), Input(1))(0) == 1)
  assert(runProgram(Program(3,9,7,9,10,9,4,9,99,-1,8), Input(8))(0) == 0)

  assert(runProgram(Program(3,3,1108,-1,8,3,4,3,99), Input(8))(0) == 1)
  assert(runProgram(Program(3,3,1108,-1,8,3,4,3,99), Input(9))(0) == 0)

  assert(runProgram(Program(3,3,1107,-1,8,3,4,3,99), Input(1))(0) == 1)
  assert(runProgram(Program(3,3,1107,-1,8,3,4,3,99), Input(8))(0) == 0)

  assert(runProgram(Program(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), Input(0))(0) == 0)
  assert(runProgram(Program(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9), Input(1))(0) == 1)

  assert(runProgram(Program(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), Input(0))(0) == 0)
  assert(runProgram(Program(3,3,1105,-1,9,1101,0,0,12,4,12,99,1), Input(1))(0) == 1)

  val out1 = runProgram(Program(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), Input(7))
  assert(out1(0) == 999)

  val out2 = runProgram(Program(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), Input(8))
  assert(out2(0) == 1000)

  val out3 = runProgram(Program(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99), Input(9))
  assert(out3(0) == 1001)
}