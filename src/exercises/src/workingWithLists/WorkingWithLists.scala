
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

    val list4 = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
//    println(compress(list4))
//    println(pack(list4))
//    println(encode(list4))
//    println(encode2(list4))
//    println(encodeModified(list4))

    val list5 = List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))
    println(decode(list5))

    println(encodeDirect(list4))

	}
}
