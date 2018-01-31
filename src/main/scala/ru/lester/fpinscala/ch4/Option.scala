package ru.lester.fpinscala.ch4

/**
 * Created with IntelliJ IDEA.
 * User: lester
 * Date: 18.10.14
 * Time: 5:14
 */

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] = this map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] = this flatMap(a => if(f(a)) Some(a) else None)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = if(xs.isEmpty) None else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def abs0: Option[Double] => Option[Double] = lift(math.abs)

  //def map2[A, B, C](a: Option[A], b: Option[B]) (f: (A, B) => C): Option[C] = for {
  //  a1 <- a
  //  b1 <- b
  //} yield f(a1, b1)

  def map2[A, B, C](a: Option[A], b: Option[B]) (f: (A, B) => C): Option[C] = a.flatMap(a1 => b.map(b1 => f(a1, b1)))

  def sequence[A](opts: List[Option[A]]): Option[List[A]] = opts match {
    case Nil => Some(Nil)
    case (h :: t) => h.flatMap(hh => sequence(t) map (hh :: _))
  }

  def sequencef[A](opts: List[Option[A]]): Option[List[A]] = opts.foldRight[Option[List[A]]](Some(Nil)) ((a, z) => z.flatMap(xs => a.map(_ :: xs)))
  def sequencef_1[A](opts: List[Option[A]]): Option[List[A]] = opts.foldRight[Option[List[A]]](Some(Nil)) ((a, z) => map2(a, z)(_ :: _))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as match {
    case Nil => Some(Nil)
    case (x :: xs) => map2(f(x), traverse(xs)(f))(_ :: _)
  }

  def traversef[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as.foldRight[Option[List[B]]](Some(Nil)) ((a, z) => z.flatMap(zz => f(a).map(aa => aa :: zz)))
  def traversef_1[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = as.foldRight[Option[List[B]]](Some(Nil)) ((a, z) => map2(f(a), z)(_ :: _))

  def sequencet[A](opts: List[Option[A]]): Option[List[A]] = traverse(opts)(identity)
}
