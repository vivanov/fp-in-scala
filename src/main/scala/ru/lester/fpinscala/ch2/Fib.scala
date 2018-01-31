package ru.lester.fpinscala.ch2

object Fib {

  def fib(n: Int): Int = {
  	@annotation.tailrec
  	def go(cur: Int, next: Int, cnt: Int): Int = {
  		println(s"cur: $cur, next: $next, cnt: $cnt")
  		if (cnt == n) cur
  		else go(next, cur + next, cnt + 1)
  	}
    go(0, 1, 0)
  }
  

  def main(args: Array[String]): Unit = {
    val res = fib(5)
    println(s"5-th number of Fibonacci sequence is: $res")
  }
}