package adventofcode

import scala.collection.mutable.ArraySeq
import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.ListBuffer
import adventofcode.Day5.Computer

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
  val OP_HALT = 99

  case class Computer(
    program: Program, input: Input = ListBuffer.empty, output: Output = ListBuffer.empty) {

    def read(): Int = input.remove(0)

    def write(value: Int): Unit = output.append(value)

    def param(mode: Int, current: Int): Int = 
      mode match {
        case POSITION_MODE => program(program(current))
        case IMMEDIATE_MODE => program(current)
      }

    def operation(mode: Int, current: Int, operation: (Int, Int) => Int) {
      val a = param(mode % 10, current + 1)
      val b = param((mode / 10) % 10, current + 2)
      update(program(current + 3), operation(a, b))
    }

    def update(current: Int, value: Int) = 
      program.update(current, value)

    @tailrec
    final def run(current: Int = 0): Program = {
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
          val p = param(IMMEDIATE_MODE, current + 1)
          update(p, a)
          run(current + 2)
        }
        case OP_OUTPUT => {
          val a = param(POSITION_MODE, current + 1)
          write(a)
          run(current + 2)
        }
        case OP_HALT => program
      }
    }

    def print() {
      println("computer status:")
      println(program)
      println(input)
      println(output)
      println()
    }
  }

  var program = Source.fromFile("src/main/resources/input-day5.txt").getLines
    .flatMap(_.split(',')).map(_.toInt).to[ArraySeq]
}

object Day5Part1 extends App {
  import Day5._

  println("Day5 Part1")

  val c = Computer(program, Input(1))
  c.run()
  c.print
}

object Day5Test extends App {
  import Day5._
  
  val c1 = Computer(Program(3, 0, 4, 0, 99), Input(1))
  c1.run()
  c1.print()
  assert(c1.output(0) == 1)

  val c2 = Computer(Program(1002, 4, 3, 4, 33))
  c2.run()
  c2.print()
  assert(c2.program(4) == 99)

  val c3 = Computer(Program(1101, 100, -1, 4, 0))
  c3.run()
  c3.print()
  assert(c3.program(4) == 99)
}