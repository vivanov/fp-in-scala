package ru.lester.fpinscala.ch2

/**
 * Created with IntelliJ IDEA.
 * User: lester
 * Date: 06.10.14
 * Time: 0:09
 */
object Compose {
  def compose[A, B, C](g: B => C, f: A => B): A => C = {
    a => g(f(a))
  }
}
