package adventofcode

object Day9Part1 extends App {
  import Day5.{runProgram, loadProgram, Input}

  println("Day9 Part1")

  val program = loadProgram("input-day9.txt")

  println(runProgram(program, Input(1)).output(0))
}

object Day9Part2 extends App {
  import Day5.{runProgram, loadProgram, Input}

  println("Day9 Part2")

  val program = loadProgram("input-day9.txt")

  println(runProgram(program, Input(2)).output(0))
}

object Day9Test extends App {
  import Day5._

  val program1 = Program(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
  assert(runProgram(program1).output == Output(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))

  val program2 = Program(1102,34915192,34915192,7,4,7,99,0)
  assert(runProgram(program2).output(0) == 1219070632396864L)

  val program3 = Program(104,1125899906842624L,99)
  assert(runProgram(program3).output(0) == 1125899906842624L)

  println("OK")
}