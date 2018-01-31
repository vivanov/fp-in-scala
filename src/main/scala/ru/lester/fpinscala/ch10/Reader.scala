package ru.lester.fpinscala.ch10

case class Reader[R, A](run: R => A)

object Reader {
  implicit def readerMonad[R] = new Monad[({type l[A] = Reader[R, A]})#l] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)
    override def flatMap[A, B](rd: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = Reader(r => f(rd.run(r)).run(r))
  }
}
