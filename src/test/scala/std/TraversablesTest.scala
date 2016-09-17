package std

/**
  * Created by Cindy.Wang on 9/17/16.
  */

import org.scalatest._


object TraversablesTest  extends FlatSpec with Matchers {
  /** At the top of the collection hierarchy is trait *Traversable*. Its only abstract operation is `foreach`:
    *
    * {{{
    * def foreach[U](f: Elem => U)
    * }}}
    *
    * Collection classes that implement *Traversable* just need to define this method; all other methods can be inherited from *Traversable*.
    *
    * The `foreach` method is meant to traverse all elements of the collection, and apply the given operation, `f`, to each element. The type of the operation is `Elem => U`, where `Elem` is the type of the collection's elements and `U` is an arbitrary result type. The invocation of `f` is done for its side effect only; in fact any function result of `f` is discarded by `foreach`.
    *
    * Traversables are the superclass of *Lists*, *Arrays*, *Maps*, *Sets*, *Streams*, and more.  The methods involved can be applied to each other in a different type. `++` appends two Traversables together.
    */
  def topOfCollectionTraversables(res0: Int, res1: Int) {
    val set = Set(1, 9, 10, 22)
    val list = List(3, 4, 5, 10)

    val result = set ++ list
    println(result.size)
    println(result)
    result.size should be(res0)

    val result2 = list ++ set
    println(result2.size)
    println(result2)
    result2.size should be(res1)
  }

  /** `map` will apply the given function on all elements of a *Traversable* and return a new collection of the result.
    */
  def mapFunctionTraversables(res0: Option[Int]) {
    val set = Set(1, 3, 4, 6)
    val result = set.map(_ * 4)
    println(result.lastOption)
    result.lastOption should be(res0)
  }

  /** `flatten` will smash all child *Traversables* within a *Traversable*
    */
  def flattenFunctionTraversables(res0: List[Int]) {
    val list = List(List(1), List(2, 3, 4), List(5, 6, 7), List(8, 9, 10))
    println(list.flatten)
    list.flatten should be(res0)
  }

  /** `flatMap` will not only apply the given function on all elements of a *Traversable*, but all elements within the elements and `flatten` the results:
    */
  def flatMapFunctionTraversables(res0: List[Int]) {
    val list = List(List(1), List(2, 3, 4), List(5, 6, 7), List(8, 9, 10))
    val result = list.flatMap(_.map(_ * 4))
    println(result)
    result should be(res0)
  }

  /** `toArray` will convert any *Traversable* to an `Array`, which is a special wrapper around a primitive *Java* array.
    */

  def toArrayFunctionTraversables(res0: Boolean) {
    val set = Set(4, 6, 7, 8, 9, 13, 14)
    val result = set.toArray
    result.isInstanceOf[Array[Int]] should be(res0)
  }

  def main(args: Array[String]): Unit = {

    topOfCollectionTraversables(7 , 8)

    mapFunctionTraversables(Some(24))

    flattenFunctionTraversables(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))

    flatMapFunctionTraversables(List(4, 8, 12, 16, 20, 24, 28, 32, 36, 40))

    toArrayFunctionTraversables(true)
  }
}
