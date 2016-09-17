package std

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Cindy.Wang on 9/17/16.
  */
object TraversablesTest2 extends FlatSpec with Matchers{
  /** `/:` or `foldLeft` will combine an operation starting with a seed and combining from the left.  *Fold Left* is defined as (seed /: list), where seed is the initial value.  Once the fold is established, you provide a function that takes two arguments.  The first argument is the running total of the operation, and the second element is the next element of the list.
    *
    * Given a `Traversable (x1, x2, x3, x4)`, an initial value of `init`, an operation `op`, `foldLeft` is defined as: `(((init op x1) op x2) op x3) op x4)`
    */
  def foldLeftFunctionTraversables(res0: Int, res1: Int, res2: Int, res3: Int, res4: Int) {
    val list = List(5, 4, 3, 2, 1)
    val result = (0 /: list) {
      (`running total`, `next element`) ⇒ `running total` - `next element`
    }
    println(result)
    result should be(res0)

    val result2 = list.foldLeft(0) {
      (`running total`, `next element`) ⇒ `running total` - `next element`
    }
    println(result2)
    result2 should be(res1)

    val result3 = (0 /: list)(_ - _) //Short hand
    println(result3)
    result3 should be(res2)

    val result4 = list.foldLeft(0)(_ - _)
    result4 should be(res3)

    (((((0 - 5) - 4) - 3) - 2) - 1) should be(res4)
  }

  /** `mkString` will also take a beginning and ending string to surround the list.
    */
  def mkStringFunctionIITraversables(res0: String) {
    val list = List(1, 2, 3, 4, 5)

    println(list.mkString(">", ",", "<"))
    list.mkString(">", ",", "<") should be(res0)
  }

  /** You would choose *foldLeft/reduceLeft* or *foldRight/reduceRight* based on your mathematical goal. One other reason for deciding is performance.  `foldLeft` is more performant since it uses tail recursion and is optimized. This exercise will either work or you will receive a *StackOverflowError*.
    */
  def performantTraversables(res0: Boolean) {
    val MAX_SIZE = 1000000
    val reduceLeftStartTime = new java.util.Date
    (1 to MAX_SIZE) reduceLeft (_ + _)
    val reduceLeftEndTime = new java.util.Date

    val reduceRightStartTime = new java.util.Date
    (1 to MAX_SIZE) reduceRight (_ + _)
    val reduceRightEndTime = new java.util.Date

    val totalReduceLeftTime = reduceLeftEndTime.getTime - reduceLeftStartTime.getTime
    val totalReduceRightTime = reduceRightEndTime.getTime - reduceRightStartTime.getTime

    (totalReduceRightTime > totalReduceLeftTime) should be(res0)
  }

  def main(args: Array[String]): Unit = {
    foldLeftFunctionTraversables(-15,-15,-15,-15,-15)

    mkStringFunctionIITraversables(">1,2,3,4,5<")

    performantTraversables(true)
  }

}
