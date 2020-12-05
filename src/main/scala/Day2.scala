package adventofcode

import scala.io.Source
import scala.collection.mutable.ArraySeq
import scala.annotation.tailrec

object Day2 {

  @tailrec
  def computer(program: ArraySeq[Int], current: Int = 0): ArraySeq[Int] = {
    program(current) match {
      case 1 => {
        val a = program(program(current + 1))
        val b = program(program(current + 2))
        program.update(program(current + 3), a + b)
        computer(program, current + 4)
      }
      case 2 => {
        val a = program(program(current + 1))
        val b = program(program(current + 2))
        program.update(program(current + 3), a * b)
        computer(program, current + 4)
      }
      case 99 => program
    }
  }

  var program = Source.fromFile("src/main/resources/input-day2.txt").getLines
    .flatMap(_.split(',')).map(_.toInt).toArray

  def fix(noun: Int, verb: Int): ArraySeq[Int] = {
    val fixed = program.to[ArraySeq]
    fixed.update(1, noun)
    fixed.update(2, verb)
    fixed
  }
}

object Day2Par1 extends App {
  import Day2._

  println("Day2 Part1")

  println(computer(fix(12, 2))(0))
}

object Day2Par2 extends App {
  import Day2._

  println("Day2 Part2")

  val inputs = for {
    noun <- 0 to 99
    verb <- 0 to 99
  } yield fix(noun, verb)

  val (noun, verb, _) = inputs.map(i => (i(1), i(2), computer(i)(0))).find(p => p._3 == 19690720).orNull

  println(100 * noun + verb)
}

object Day2Test extends App {
  import Day2._

  assert(computer(ArraySeq(1,0,0,0,99)) == ArraySeq(2,0,0,0,99))
  assert(computer(ArraySeq(2,3,0,3,99)) == ArraySeq(2,3,0,6,99))
  assert(computer(ArraySeq(2,4,4,5,99,0)) == ArraySeq(2,4,4,5,99,9801))
  assert(computer(ArraySeq(1,1,1,4,99,5,6,0,99)) == ArraySeq(30,1,1,4,2,5,6,0,99))
}