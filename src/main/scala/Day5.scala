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

  case class Computer(
      program: Program, 
      input: Input = Input.empty, 
      output: Output = Output.empty) {

    def read(): Int = input.remove(0)

    def write(value: Int): Unit = output.append(value)

    def param(mode: Int, current: Int): Int = 
      mode match {
        case POSITION_MODE => program(program(current))
        case IMMEDIATE_MODE => program(current)
      }

    def operation(mode: Int, current: Int, operation: (Int, Int) => Int): Int = {
      val a = param(mode1(mode), current + 1)
      val b = param(mode2(mode), current + 2)
      update(program(current + 3), operation(a, b))
      current + 4
    }

    def jump(mode: Int, current: Int, operation: Int => Boolean): Int = {
      val a = param(mode1(mode), current + 1)
      val b = param(mode2(mode), current + 2)
      if (operation(a)) b else current + 3
    }

    def comparation(mode: Int, current: Int, compartion : (Int, Int) => Boolean): Int = {
      val a = param(mode1(mode), current + 1)
      val b = param(mode2(mode), current + 2)
      val c = program(current + 3)
      if (compartion(a, b)) 
        update(c, 1)
      else
        update(c, 0)
      current + 4
    }

    def update(current: Int, value: Int) = 
      program.update(current, value)

    def mode1(mode: Int) = mode % 10
    def mode2(mode: Int) = mode1(mode / 10)

    @tailrec
    final def run(current: Int = 0): Output = {
//      debug(current)
      val command = program(current)
      val mode = command / 100
      command % 100 match {
        case OP_SUM => {
          run(operation(mode, current, (a, b) => a + b))
        }
        case OP_MULTIPLY => {
          run(operation(mode, current, (a, b) => a * b))
        }
        case OP_INPUT => {
          println("read: " + current)
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
          run(jump(mode, current, _ != 0))
        }
        case OP_JUMP_IF_FALSE => {
          run(jump(mode, current, _ == 0))
        }
        case OP_LESS_THAN => {
          run(comparation(mode, current, _ < _))
        }
        case OP_EQUALS => {
          run(comparation(mode, current, _ == _))
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

  def loadProgram(input: String): Array[Int] =
    Source.fromFile(s"src/main/resources/$input").getLines
      .flatMap(_.split(',')).map(_.toInt).toArray

  var program = loadProgram("input-day5.txt")

  def runProgram(program: Array[Int], input: Input = Input.empty): Output = 
    Computer(program.to[ArraySeq], input).run()

  def runProgram2(program: ArraySeq[Int], input: Input = Input.empty): Output = 
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
  
  assert(runProgram(program = Array(3, 0, 4, 0, 99), Input(1))(0) == 1)

  val c2 = Computer(Program(1002, 4, 3, 4, 33))
  c2.run()
  assert(c2.program(4) == 99)

  val c3 = Computer(Program(1101, 100, -1, 4, 0))
  c3.run()
  assert(c3.program(4) == 99)

  val program1 = Array(3,9,8,9,10,9,4,9,99,-1,8)
  assert(runProgram(program1, Input(8))(0) == 1)
  assert(runProgram(program1, Input(9))(0) == 0)

  val program2 = Array(3,9,7,9,10,9,4,9,99,-1,8)
  assert(runProgram(program2, Input(1))(0) == 1)
  assert(runProgram(program2, Input(8))(0) == 0)

  val program3 = Array(3,3,1108,-1,8,3,4,3,99)
  assert(runProgram(program3, Input(8))(0) == 1)
  assert(runProgram(program3, Input(9))(0) == 0)

  val program4 = Array(3,3,1107,-1,8,3,4,3,99)
  assert(runProgram(program4, Input(1))(0) == 1)
  assert(runProgram(program4, Input(8))(0) == 0)

  val program5 = Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
  assert(runProgram(program5, Input(0))(0) == 0)
  assert(runProgram(program5, Input(1))(0) == 1)

  val program6 = Array(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
  assert(runProgram(program6, Input(0))(0) == 0)
  assert(runProgram(program6, Input(1))(0) == 1)

  val programX = Array(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
  assert(runProgram(programX, Input(7))(0) == 999)
  assert(runProgram(programX, Input(8))(0) == 1000)
  assert(runProgram(programX, Input(9))(0) == 1001)
}