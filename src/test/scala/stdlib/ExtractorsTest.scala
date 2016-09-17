package std

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Cindy.Wang on 9/18/16.
  */
object ExtractorsTest extends FlatSpec with Matchers {

  /** In this exercise we use the unapply for pattern matching employee objects
    */
  def unapplyForPatternMatchingExtractors(res0: String) {
    class Employee(
                    val firstName:  String,
                    val middleName: Option[String],
                    val lastName:   String
                  )

    object Employee {
      //factory methods, extractors, apply
      //Extractor: Create tokens that represent your object
      def unapply(x: Employee) =
      Some(x.lastName, x.middleName, x.firstName)
    }

    val singri = new Employee("Singri", None, "Keerthi")

    println(singri.firstName)
    println(singri.middleName)
    println(singri.lastName)

    val result = singri match {
      case Employee("Singri", None, x)    ⇒ "Yay, Singri %s! with no middle name!".format(x)
      case Employee("Singri", Some(x), _) ⇒ "Yay, Singri with a middle name of %s".format(x)
      case _                              ⇒ "I don't care, going on break"
    }

    println(result)
    result should be(res0)
  }

  def main(args: Array[String]): Unit = {
    unapplyForPatternMatchingExtractors("I don't care, going on break")
  }
}
