package adventofcode

object Day4 {
  def valid(input: String): Boolean = {
    val a = input.dropRight(1)
    val b = input.drop(1)
    val pairs = a.zip(b)

    input.size == 6 && pairs.forall(p => p._2 >= p._1) && !pairs.filter(p => p._1 == p._2).isEmpty
  }

  def passwords(begin: Int, end: Int): Int = {
    (begin to end).map(_.toString()).filter(valid).size
  }
}

object Day4Part1 extends App {
  import Day4._

  println(passwords(152085, 670283))
}

object Day4Test extends App {
  import Day4._

  assert(valid("123455"))
  assert(valid("112233"))
  assert(!valid("112230"))
}