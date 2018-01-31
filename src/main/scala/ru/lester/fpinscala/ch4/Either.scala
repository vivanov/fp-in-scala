package ru.lester.fpinscala.ch4

/**
 * Created with IntelliJ IDEA.
 * User: lester
 * Date: 22.10.14
 * Time: 17:25
 */


object Either {

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(a) => Right(a)
    }

    //def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {aa <- this; bb <- b} yield f(aa, bb)
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this.flatMap(aa => b.map(bb => f(aa, bb)))
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def Try[A](a: => A): Either[Exception, A] =
    try{ Right(a)}
    catch {case e: Exception => Left(e)}

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case x :: xs => x flatMap (a => sequence(xs) map (a :: _))
  }

  def sequence_1[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil => Right(Nil)
    case x :: xs => x.map2(sequence_1(xs))(_ :: _)
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case x :: xs => f(x) flatMap (xx => traverse(xs)(f) map (xx :: _))
  }

  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case x :: xs => f(x).map2(traverse(xs)(f))(_ :: _)
  }
}
