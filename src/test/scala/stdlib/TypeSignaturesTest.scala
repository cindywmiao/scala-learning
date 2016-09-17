package std

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by Cindy.Wang on 9/18/16.
  */
object TypeSignaturesTest extends FlatSpec with Matchers {

  /** Class meta-information can be retrieved by class name by using `classOf[className]`
    */
  def retrieveMetaInformationTypeSignatures(res0: String, res1: String) {
    println(classOf[String].getCanonicalName)
    println(classOf[String].getSimpleName)

    classOf[String].getCanonicalName should be(res0)
    classOf[String].getSimpleName should be(res1)
  }

  /** Class meta-information can be derived from an object reference using `getClass()`
    */
  def deriveMetaInformationTypeSignatures(res0: Boolean, res1: String, res2: String) {
    val zoom = "zoom"
    zoom.isInstanceOf[String] should be(res0)
    zoom.getClass.getCanonicalName should be(res1)
    zoom.getClass.getSimpleName should be(res2)
  }

  /** `isInstanceOf[className]` is used to determine the if an object reference is an instance of given class:
    */
  def isInstanceOfFunctionTypeSignatures(res0: Boolean) {
    trait Randomizer[A] {
      def draw(): A
    }

    class IntRandomizer extends Randomizer[Int] {
      def draw() = {
        import util.Random
        Random.nextInt()
      }
    }

    val intRand = new IntRandomizer
    intRand.draw.isInstanceOf[Int] should be(res0)
  }

  def main(args: Array[String]): Unit = {
    retrieveMetaInformationTypeSignatures("java.lang.String", "String")
  }

}
