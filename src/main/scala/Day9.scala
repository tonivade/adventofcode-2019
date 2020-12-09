package adventofcode

object Day9 {

}

object Day9Test extends App {
  import Day5._

  val program1 = Program(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
  println(runProgram(program1).output == Output(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))

  val program2 = Program(1102,34915192,34915192,7,4,7,99,0)
  println(runProgram(program2))

//  val program3 = Program(104,1125899906842624L,99)
//  println(runProgram(program3))
}