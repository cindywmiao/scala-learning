package workingWithLists

/**
  * Created by Cindy.Wang on 10/10/16.
  */
object Group {
  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl => combinations(n - 1, sl.tail) map {sl.head :: _} }

  def group3[A](ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(2, ls)
      noA = ls -- a
      b <- combinations(3, noA)
    } yield List(a, b, noA -- b)

  def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
    case Nil     => List(Nil)
    case n :: ns => combinations(n, ls) flatMap { c =>
      group(ns, ls -- c) map {c :: _}
    }
  }
}
