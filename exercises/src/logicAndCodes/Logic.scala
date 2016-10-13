//package logicAndCodes

/**
  * Created by Cindy.Wang on 10/13/16.
  */
object Logic {
  def not(a: Boolean) : Boolean = a match {
    case true => false
    case false => true
  }

  def and(a: Boolean, b: Boolean) : Boolean = (a, b) match {
    case(true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean) : Boolean = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def equ(a: Boolean, b: Boolean) : Boolean = or(and(a, b), and(not(a), not(b)))

  def xor(a: Boolean, b: Boolean) : Boolean = not(equ(a, b))

  def nor(a: Boolean, b: Boolean) : Boolean = not(or(a, b))

  def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))

  def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A     B     result")
    for {a <- List(true, false);
         b <- List(true, false)} {
      printf("%-5s %-5s %-5s\n", a, b, f(a, b))
    }
  }


  def main(args: Array[String]): Unit = {
    table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    println("Hello World")
  }

}
