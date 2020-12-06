package adventofcode

object Day4 {
  
  def valid(check: Int => Boolean)(input: String): Boolean = {
    val a = input.dropRight(1)
    val b = input.drop(1)
    val pairs = a.zip(b)

    input.size == 6 && 
      pairs.forall(p => p._2 >= p._1) && 
      input.toSet.exists(c => check(input.count(_ == c)))
  }

  def passwords(begin: Int, end: Int)(validator: String => Boolean): Int = 
    (begin to end).map(_.toString()).filter(validator).size
}

object Day4Part1 extends App {
  import Day4._

  println("Day4 Part1")

  println(passwords(152085, 670283)(valid(_ >= 2)))
}

object Day4Part2 extends App {
  import Day4._

  println("Day4 Part2")

  println(passwords(152085, 670283)(valid(_ == 2)))
}

object Day4Test extends App {
  import Day4._

  assert(valid(_ >= 2)("123455"))
  assert(valid(_ >= 2)("112233"))
  assert(!valid(_ >= 2)("112230"))

  assert(valid(_ == 2)("112233"))
  assert(!valid(_ == 2)("123444"))
  assert(valid(_ == 2)("111122"))
}