
object WorkingWithLists{

  def last[A](ls: List[A]): A = ls.last

	def penultimate[A](ls: List[A]): A ={
    if (ls.isEmpty) throw new NoSuchElementException
    else ls.init.last
  }

	def nth[A](n: Int, ls: List[A]): A =
    if (n >= 0) ls(n)
    else throw new NoSuchElementException

	def length[A](list: List[A]) : Int = list.length

	def reverse[A](list: List[A]) : List[A] = list.reverse

	def isPalindrome[A](list : List[A]) : Boolean = list == list.reverse

	def flatten[A](ls: List[Any]): List[Any] = ls flatMap {
  		case i: List[_] => flatten(i)
  		case e => List(e)
	}

	def compress[A](list : List[A]) : List[A] = list.foldRight(List[A]()){ (h, r) => if(r.isEmpty || r.head != h) h::r else r }

  def pack[A](ls : List[A]): List[List[A]] = {
    if(ls.isEmpty) List(List())
    else{
      val(packed, next) = ls span {_ == ls.head}
      if(next == Nil) List(packed) else packed :: pack(next)
    }
  }

  def encode[A](ls : List[A]): List[(Int, A)] = {
    if(ls.isEmpty) List()
    else{
      val(packed, next) = ls span {_ == ls.head}
      if(next == Nil) List((packed.length, packed.head)) else (packed.length, packed.head) :: encode(next)
    }
  }

  def encode2[A](ls: List[A]): List[(Int, A)] = pack(ls) map { e => (e.length, e.head) }

  def encodeModified[A](ls : List[A]) : List[Any] = encode(ls) map { e => if(e._1 == 1) e._2 else e }

  def encodeModified2[A](ls: List[A]): List[Either[A, (Int, A)]] =
    encode(ls) map { t => if (t._1 == 1) Left(t._2) else Right(t) }

  def decode[A](ls: List[(Int, A)]): List[A] =
    ls flatMap { e => List.fill(e._1)(e._2) }

  def encodeDirect[A](ls : List[A]): List[(Int, A)] = {
    if(ls.isEmpty) List()
    else{
      val(packed, next) = ls span {_ == ls.head}
      (packed.length, packed.head) :: encodeDirect(next)
    }
  }

  def duplicate[A](ls : List[A]) : List[A] = ls flatMap {e => List(e, e)}

  def dupulicateN[A](n: Int, ls: List[A]) : List[A] = ls flatMap {e => List.fill(n)(e)}

  def drop[A](n: Int, ls: List[A]) : List[A] = ls.zipWithIndex filter { v => (v._2 + 1) % n != 0 } map { _._1 }

  def split[A](n: Int, ls: List[A]) : (List[A], List[A]) = (ls.take(n), ls.drop(n))

  def slice[A](m: Int, n: Int, ls: List[A]) : List[A] = ls.drop(m).take(n - m)

  def rotate[A](n: Int, ls: List[A]) : List[A] = {
    val nBounded = if (ls.isEmpty) 0 else n % ls.length
    if(nBounded > 0) (ls drop nBounded) ::: (ls take nBounded)
    else ls.drop(ls.length + nBounded) ::: (ls.take(ls.length + nBounded))
  }

  def removeAt[A](n: Int, ls: List[A]): (List[A], A) = (((ls take n) ::: (ls drop n + 1)), ls(n))

  def insertAt[A](e: A, n: Int, ls: List[A]): List[A] = {
    ls.splitAt(n) match {
      case (pre, post) => pre ::: e :: post
    }
  }

  def range(start : Int, end : Int): List[Int] = List.range(start, end + 1)

  def randomSelect[A](n: Int, ls: List[A]): List[A] ={
    if(n <= 0) Nil
    else{
      val (rest, e) = removeAt((new util.Random).nextInt(ls.length), ls)
      e :: randomSelect(n - 1, rest)
    }
  }

  def lotto(count : Int, max : Int) : List[Int] = randomSelect(count, List.range(1, max + 1))

  def randomPermute[A](ls : List[A]) : List[A] = randomSelect(ls.length, ls)

  def flatMapSublists[A,B](ls: List[A])(f: (List[A]) => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { sl => combinations(n - 1, sl.tail) map {sl.head :: _} }

//  def group3[A](ls: List[A]): List[List[List[A]]] =
//    for {
//      a <- combinations(2, ls)
//      noA = ls -- a
//      b <- combinations(3, noA)
//    } yield List(a, b, noA -- b)
//
//  def group[A](ns: List[Int], ls: List[A]): List[List[List[A]]] = ns match {
//    case Nil     => List(Nil)
//    case n :: ns => combinations(n, ls) flatMap { c =>
//      group(ns, ls -- c) map {c :: _}
//    }
//  }

//  def lsort[A](ls: List[List[A]]): List[List[A]] =
//    ls sort { _.length < _.length }
//
//  def lsortFreq[A](ls: List[List[A]]): List[List[A]] = {
//    val freqs = Map(encode(ls map { _.length } sort { _ < _ }) map { _.swap }:_*)
//    ls sort { (e1, e2) => freqs(e1.length) < freqs(e2.length) }
//  }

	def main(args: Array[String]): Unit = {
//		val list =  List(1, 1, 2, 3, 5, 8)
//		println(penultimate(list))
//		println(nth(2, list))
//		println(length(list))
//		println(reverse(list))
//
//		val list2 = List(1, 2, 3, 2, 1)
//		println(isPalindrome(list))
//		println(isPalindrome(list2))
//
//		val list3 = List(List(1, 1), 2, List(3, List(5, 8)))
//		println(flatten(list3))

//    val list4 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
//    println(compress(list4))
//    println(pack(list4))
//    println(encode(list4))
//    println(encode2(list4))
//    println(encodeModified(list4))

//    val list5 = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
//    println(decode(list5))
//    println(encodeDirect(list4))
//
//    val list6 = List('a, 'b, 'c, 'c, 'd)
//    println(duplicate(list6))
//    println(dupulicateN(3, list6))
//
//    val list7 = List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)
//    println(drop(3, list7))
//    println(split(3, list7))
//    println(slice(3, 7, list7))
//    println(slice2(3, 7, list7))
//    println(rotate(3, list7))
//    println(rotate(-2, list7))

    val list8 = List('a, 'b, 'c, 'd)
    println(removeAt(1, list8))
    println(insertAt('new, 1, list8))

    println(range(4,7))
    println(lotto(6, 47))
    println(randomPermute(list8))
    println(combinations(3, list8))

    val list9 = List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")
    //println(group3(list9))
	}
}
