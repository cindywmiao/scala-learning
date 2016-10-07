object WorkingWithLists{

	/*Find the last element of a list.*/
	// There are several ways to solve this problem.  If we use builtins, it's very easy.
  	def lastBuiltin[A](ls: List[A]): A = ls.last

  	// The standard functional approach is to recurse down the list until we hit
  	// the end.  Scala's pattern matching makes this easy.
  	def lastRecursive[A](ls: List[A]): A = ls match {
    	case h :: Nil  => h
    	case _ :: tail => lastRecursive(tail)
    	case _         => throw new NoSuchElementException
  	}

	/*Find the last but one element of a list.*/
	def penultimateBuiltin[A](ls: List[A]): A =
    	if (ls.isEmpty) throw new NoSuchElementException
    	else ls.init.last

	def penultimateRecursive[A](ls : List[A]) : A = ls match {
		case h :: _ :: Nil => h
    	case _ :: tail     => penultimateRecursive(tail)
    	case _             => throw new NoSuchElementException
	}
	def lastNthBuiltin[A](n: Int, ls: List[A]): A = {
    	if (n <= 0) throw new IllegalArgumentException
    	if (ls.length < n) throw new NoSuchElementException
    	ls.takeRight(n).head
  	}

	def lastNthRecursive[A](n: Int, ls: List[A]): A = {
    	def lastNthR(count: Int, resultList: List[A], curList: List[A]): A = curList match {
        	case Nil if count > 0 => throw new NoSuchElementException
        	case Nil              => resultList.head
        	case _ :: tail        =>
          		lastNthR(count - 1,
                   if (count > 0) resultList else resultList.tail,
                   tail)
      	}
    	if (n <= 0) throw new IllegalArgumentException
    	else lastNthR(n, ls, ls)
  	}

	/*Find the Kth element of a list.
	By convention, the first element in the list is element 0.*/
	// Trivial with builtins.
  	def nthBuiltin[A](n: Int, ls: List[A]): A =
    	if (n >= 0) ls(n)
    	else throw new NoSuchElementException

  	// Not that much harder without.
  	def nthRecursive[A](n: Int, ls: List[A]): A = (n, ls) match {
    	case (0, h :: _   ) => h
    	case (n, _ :: tail) => nthRecursive(n - 1, tail)
    	case (_, Nil      ) => throw new NoSuchElementException
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
		println(lastBuiltin(list))
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
