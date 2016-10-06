object WorkingWithLists{

	/*Find the last element of a list.*/
	def last(list : List[Int]) : Int = {
		list.last
	}

	/*Find the last but one element of a list.*/
	def penultimate(list : List[Int]) : Int = {
		if(list == Nil || list.length <= 1)
			0
		else
			list(list.length - 2)
	}

	/*Find the Kth element of a list.
	By convention, the first element in the list is element 0.*/
	def nth(index : Int, list : List[Int]) : Int = {
		if(list == Nil || list.length == 0 || list.length < index) 
			0
		else 
			list(index)
	}

	/*Find the number of elements of a list.*/
	def length(list: List[Int]) : Int = {
		list.length
	}

	/*Reverse a list.*/
	def reverse(list: List[Int]) : List[Int] = {
		list.reverse
	}

	/*Find out whether a list is a palindrome.*/
	def isPalindrome(list : List[Int]) : Boolean = {
		list == list.reverse
	}

	/*Flatten a nested list structure.*/
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
