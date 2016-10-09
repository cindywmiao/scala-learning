package WorkingWithLists

object WorkingWithLists{

  def last[A](ls: List[A]): A = ls.last

	def penultimate[A](ls: List[A]): A =
    if (ls.isEmpty) throw new NoSuchElementException
    else ls.init.last

	def nth[A](n: Int, ls: List[A]): A =
    if (n >= 0) ls(n)
    else throw new NoSuchElementException

	def length[A](list: List[A]) : Int = list.length

	def reverse[A](list: List[A]) : List[A] = list.reverse

	def isPalindrome[A](list : List[A]) : Boolean = list == list.reverse

	def flatten(ls: List[Any]): List[Any] = ls flatMap {
  		case i: List[_] => flatten(i)
  		case e => List(e)
	}

	/*Eliminate consecutive duplicates of list elements.
	If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.*/
	def compress(list : List[Char]) : List[Char] = {
		list
	}


	def main(args: Array[String]): Unit = {
		val list =  List(1, 1, 2, 3, 5, 8)
		println(last(list))
		println(penultimate(list))
		println(nth(2, list))
		println(length(list))
		println(reverse(list))
		
		val list2 = List(1, 2, 3, 2, 1)
		println(isPalindrome(list))
		println(isPalindrome(list2))

		val list3 = List(List(1, 1), 2, List(3, List(5, 8)))
		println(flatten(list3))
	}
}
