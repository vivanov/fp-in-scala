package ru.lester.fpinscala.ch2

/**
 * Created with IntelliJ IDEA.
 * User: lester
 * Date: 06.10.14
 * Time: 0:02
 */
object Curry {
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A, B, C](f: A => B  => C): (A, B) => C = {
    (a, b) => f(a) (b)
  }
}
