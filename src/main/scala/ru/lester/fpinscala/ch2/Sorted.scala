package ru.lester.fpinscala.ch2

object Sorted {
	def isSorted[A](arr: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
	  def loop(n: Int): Boolean = {
	    if(n == arr.length) true
	    else if(!ordered(arr(n - 1), arr(n))) false
	    else loop(n + 1)
	  }
      if(arr.length <= 1) true
      else loop(1)
	}

  def main(args: Array[String]): Unit = {
    val res = isSorted(Array(4,8,11), (el1: Int, el2: Int) => el1 < el2)
    println(s"""Array(4,8,11) is: ${if(res) "Sorted" else "Not sorted"}""")

    val res2 = isSorted(Array(15,2,6), (el1: Int, el2: Int) => el1 < el2)
    println(s"""Array(15,2,6) is: ${if(res2) "Sorted" else "Not sorted"}""")
  }
}