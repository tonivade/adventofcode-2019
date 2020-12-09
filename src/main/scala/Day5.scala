package adventofcode

import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

object Day5 {

  type Program = Array[Int]
  type Memory = ArrayBuffer[Int]
  type Input = ListBuffer[Int]
  type Output = ListBuffer[Int]
  
  val Program = Array
  val Memory = ArrayBuffer
  val Input = ListBuffer
  val Output = ListBuffer

  val POSITION_MODE = 0
  val IMMEDIATE_MODE = 1
  val RELATIVE_MODE = 2

  val OP_SUM = 1
  val OP_MULTIPLY = 2
  val OP_INPUT = 3
  val OP_OUTPUT = 4
  val OP_JUMP_IF_TRUE = 5
  val OP_JUMP_IF_FALSE = 6
  val OP_LESS_THAN = 7
  val OP_EQUALS = 8
  val OP_INCR_RELATIVE = 9
  val OP_HALT = 99

  case class State(
      current: Int, 
      base: Int,
      memory: Memory,
      input: Input, 
      output: Output, 
      paused: Boolean = false, 
      stopped: Boolean = false) {

    def resume(newInput: Input): State = 
      if (paused)
        Computer(memory, newInput).run(current)
      else 
        throw new IllegalStateException()
  }

  case class Computer(
      memory: Memory, 
      input: Input = Input.empty, 
      output: Output = Output.empty) {

    def read(): Option[Int] = 
      if (input.isEmpty) 
        None
      else
        Some(input.remove(0))

    def write(value: Int): Unit = output.append(value)

    def param(mode: Int, current: Int, base: Int): Int = {
//      println(s"mode:$mode,current:$current,base:$base")
      mode match {
        case POSITION_MODE => get(get(current))
        case IMMEDIATE_MODE => get(current)
        case RELATIVE_MODE => get(base + get(current))
      }
    }

    def operation(mode: Int, current: Int, base: Int, operation: (Int, Int) => Int): Int = {
      val a = param(mode1(mode), current + 1, base)
      val b = param(mode2(mode), current + 2, base)
      update(get(current + 3), operation(a, b))
      current + 4
    }

    def jump(mode: Int, current: Int, base: Int, operation: Int => Boolean): Int = {
      val a = param(mode1(mode), current + 1, base)
      val b = param(mode2(mode), current + 2, base)
      if (operation(a)) b else current + 3
    }

    def comparation(mode: Int, current: Int, base: Int, operation: (Int, Int) => Boolean): Int = {
      val a = param(mode1(mode), current + 1, base)
      val b = param(mode2(mode), current + 2, base)
      val c = get(current + 3)
      if (operation(a, b))
        update(c, 1)
      else
        update(c, 0)
      current + 4
    }

    def update(current: Int, value: Int): Unit = {
      require(current >= 0, s"cannot write negative possitions from memory: $current")
      if (memory.length > current)
        memory.update(current, value)
      else {
        val pad = Array.fill[Int]((current - memory.length) + 1)(0)
        memory.append(pad:_*)
      }
    }

    def get(current: Int): Int = {
      require(current >= 0, s"cannot read negative possitions from memory: $current")
      if (memory.length > current)
        memory(current)
      else
        0
    }

    def mode1(mode: Int) = mode % 10
    def mode2(mode: Int) = mode1(mode / 10)

    def pause(current: Int, base: Int): State = 
      State(current, base, memory, input, output, paused = true)

    def stop(current: Int, base: Int): State = 
      State(current, base, memory, input, output, stopped = true)

    @tailrec
    final def run(current: Int = 0, base: Int = 0): State = {
//      debug(current)
      val command = get(current)
      val mode = command / 100
      command % 100 match {
        case OP_SUM => {
          run(operation(mode, current, base, (a, b) => a + b), base)
        }
        case OP_MULTIPLY => {
          run(operation(mode, current, base, (a, b) => a * b), base)
        }
        case OP_INPUT => {
          read() match {
            case Some(value) => {
              val p = get(current + 1)
              update(p, value)
              run(current + 2, base)
            }
            case None => pause(current, base)
          }
        }
        case OP_OUTPUT => {
          val a = param(mode1(mode), current + 1, base)
          write(a)
          run(current + 2, base)
        }
        case OP_JUMP_IF_TRUE => {
          run(jump(mode, current, base, _ != 0), base)
        }
        case OP_JUMP_IF_FALSE => {
          run(jump(mode, current, base, _ == 0), base)
        }
        case OP_LESS_THAN => {
          run(comparation(mode, current, base, _ < _), base)
        }
        case OP_EQUALS => {
          run(comparation(mode, current, base, _ == _), base)
        }
        case OP_INCR_RELATIVE => {
          run(current + 2, base + param(mode1(mode), current + 1, base))
        }
        case OP_HALT => stop(current, base)
      }
    }

    def debug(current: Int = 0) {
      println(s"computer pointer: $current")
      println(memory)
      println(input)
      println(output)
      println()
    }
  }

  def loadProgram(input: String): Program =
    Source.fromFile(s"src/main/resources/$input").getLines
      .flatMap(_.split(',')).map(_.toInt).toArray

  var program = loadProgram("input-day5.txt")

  def runProgram(program: Program, input: Input = Input.empty): State = 
    Computer(program.to[ArrayBuffer], input).run()
}

object Day5Part1 extends App {
  import Day5._

  println("Day5 Part1")

  println(runProgram(program, Input(1)).output)
}

object Day5Part2 extends App {
  import Day5._

  println("Day5 Part2")

  println(runProgram(program, Input(5)).output)
}

object Day5Test extends App {
  import Day5._
  
  assert(runProgram(program = Program(3, 0, 4, 0, 99), Input(1)).output(0) == 1)

  val c2 = Computer(Memory(1002, 4, 3, 4, 33))
  c2.run()
  assert(c2.memory(4) == 99)

  val c3 = Computer(Memory(1101, 100, -1, 4, 0))
  c3.run()
  assert(c3.memory(4) == 99)

  val program1 = Array(3,9,8,9,10,9,4,9,99,-1,8)
  assert(runProgram(program1, Input(8)).output(0) == 1)
  assert(runProgram(program1, Input(9)).output(0) == 0)

  val program2 = Array(3,9,7,9,10,9,4,9,99,-1,8)
  assert(runProgram(program2, Input(1)).output(0) == 1)
  assert(runProgram(program2, Input(8)).output(0) == 0)

  val program3 = Array(3,3,1108,-1,8,3,4,3,99)
  assert(runProgram(program3, Input(8)).output(0) == 1)
  assert(runProgram(program3, Input(9)).output(0) == 0)

  val program4 = Array(3,3,1107,-1,8,3,4,3,99)
  assert(runProgram(program4, Input(1)).output(0) == 1)
  assert(runProgram(program4, Input(8)).output(0) == 0)

  val program5 = Array(3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9)
  assert(runProgram(program5, Input(0)).output(0) == 0)
  assert(runProgram(program5, Input(1)).output(0) == 1)

  val program6 = Array(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)
  assert(runProgram(program6, Input(0)).output(0) == 0)
  assert(runProgram(program6, Input(1)).output(0) == 1)

  val programX = Array(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
                       1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
                       999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99)
  assert(runProgram(programX, Input(7)).output(0) == 999)
  assert(runProgram(programX, Input(8)).output(0) == 1000)
  assert(runProgram(programX, Input(9)).output(0) == 1001)

  println("OK")
}