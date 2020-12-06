package adventofcode

object Day4 {
  def group2(input: String): IndexedSeq[(Char, Char)] = {
    val a = input.dropRight(1)
    val b = input.drop(1)
    a.zip(b)
  }
  
  def group3(input: String): IndexedSeq[(Char, Char, Char)] = {
    val a = input.dropRight(2)
    val b = input.drop(1).dropRight(1)
    val c = input.drop(2)
    a.zip(b).zip(c).map(p => (p._1._1, p._1._2, p._2))
  }
  
  def group4(input: String): IndexedSeq[(Char, Char, Char, Char)] = {
    val a = input.dropRight(3)
    val b = input.drop(1).dropRight(2)
    val c = input.drop(2).dropRight(1)
    val d = input.drop(3)
    a.zip(b).zip(c).map(p => (p._1._1, p._1._2, p._2)).zip(d).map(p => (p._1._1, p._1._2, p._1._3, p._2))
  }
  
  def valid1(input: String): Boolean = {
    val pairs = group2(input)

    input.size == 6 && 
      pairs.forall(p => p._2 >= p._1) && 
      !pairs.filter(p => p._1 == p._2).isEmpty
  }
  
  def valid2(input: String): Boolean = {
    val pairs = group2(input)
    val triplets = group3(input)
    val quadruplets = group4(input)

    val x = pairs.filter(p => p._1 == p._2).map(_._1).toSet
    val y = triplets.filter(p => p._1 == p._2 && p._1 == p._3).map(_._1).toSet
    val z = quadruplets.filter(p => p._1 == p._2 && p._1 == p._3 && p._1 == p._4).map(_._1).toSet

    if (y.isEmpty)
      true
    else if (!x.intersect(y).isEmpty && x.intersect(z).isEmpty) 
      false
    else
      true
  }

  def passwords(begin: Int, end: Int)(validator: String => Boolean): Int = 
    (begin to end).map(_.toString()).filter(validator).size
}

object Day4Part1 extends App {
  import Day4._

  println("Day4 Part1")

  println(passwords(152085, 670283)(valid1))
}

object Day4Part2 extends App {
  import Day4._

  println("Day4 Part2")

  println(passwords(152085, 670283)(i => valid1(i) && valid2(i)))
}

object Day4Test extends App {
  import Day4._

  assert(valid1("123455"))
  assert(valid1("112233"))
  assert(!valid1("112230"))

  assert(valid2("112233"))
  assert(!valid2("123444"))
  assert(valid2("111122"))
}