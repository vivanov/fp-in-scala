package ru.lester.fpinscala.ch10

import ru.lester.fpinscala.ch4.{Failure, Success, Validation}

trait Applicative[F[_]] extends Functor[F] {
  def unit[A](a: => A) : F[A]
  def apply[A, B](fab: F[A => B])(fa: F[A]): F[B] = map2(fa, fab)((a, f) => f(a))
  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply[(A, B), C](unit(f.tupled)) (apply[B, (A, B)](apply[A, B => (A, B)](unit[A => (B => (A, B))](a => (b => (a, b)) ) )(fa))(fb))
  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply[(A, B, C), D](unit(f.tupled))(apply(apply(apply(unit[A => (B => (C => (A, B, C)))](a => (b => (c => (a, b, c)))))(fa))(fb))(fc))
  def map4[A, B, C, D, E](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A, B, C, D) => E): F[E] = apply[(A, B, C, D), E](unit(f.tupled))(apply(apply(apply(apply(unit[A => (B => (C => (D => (A, B, C, D))))](a => (b => (c => (d => (a, b, c, d))))))(fa))(fb))(fc))(fd))

  override def map[A, B](fa: F[A])(f: A => B): F[B] = map2(fa, unit(()))((a, _) => f(a))
  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] = as.foldRight(unit(Nil: List[B]))((a, acc) => map2(f(a), acc)(_ :: _))
  def sequence[A](as: List[F[A]]): F[List[A]] = traverse(as)(identity)
  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence((0 to n).toList.map(_ => fa))
  def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = map2(fa, fb)((_, _))
  def productap[G[_]](G: Applicative[G]): Applicative[({type l[X] = (F[X], G[X])})#l] = {
    val self = this
    new Applicative[({type l[X] = (F[X], G[X])})#l] {
      override def unit[A](a: => A): (F[A], G[A]) = (self.unit(a), G.unit(a))
      override def apply[A, B](fab: (F[A => B], G[A => B]))(fa: (F[A], G[A])): (F[B], G[B]) = (self.apply(fab._1)(fa._1), G.apply(fab._2)(fa._2))
    }
  }
  def compose[G[_]](G: Applicative[G]): Applicative[({type l[X] = F[G[X]]})#l] = {
    val self = this
    new Applicative[({type l[X] = F[G[X]]})#l] {
      override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
      override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A, B) => C): F[G[C]] = self.map2(fga, fgb)((ga, gb) => G.map2(ga, gb)(f))
    }
  }

  def sequenceMap[K, V](ofa: Map[K, F[V]]): F[Map[K, V]] = ofa.foldLeft(unit(Map.empty[K, V]))((fmkv, kfv) => map2(fmkv, kfv._2)((mkv, v) => mkv + (kfv._1 -> v)))
}

object Applicative {
  import Monoid.vectorMonoid
  def validationApplicative[E: Monoid] = new Applicative[({type l[X] = Validation[E, X]})#l] {
    override def unit[A](a: => A): Validation[E, A] = Success(a)
    //Semigroup
    override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = (fab, fa) match {
      case (Success(f), Success(a)) => Success(f(a))
      case (Failure(e1h, e1t), Failure(e2h, e2t)) => Failure(implicitly[Monoid[E]].op(e1h, e2h), implicitly[Monoid[Vector[E]]].op(e1t, e2t))
      case (f@Failure(_, _), _) => f
      case (_, f@Failure(_, _)) => f
    }
  }

  type Const[M, B] = M

  def monoidApplicative[M: Monoid] = new Applicative[({type l[X] = Const[M, X]})#l] {
    override def unit[A](a: => A): M = implicitly[Monoid[M]].zero
    override def apply[A, B](m1: M)(m2: M): M = implicitly[Monoid[M]].op(m1, m2)
    override def map2[A, B, C](m1: M, m2: M)(f: (A, B) => C): M = implicitly[Monoid[M]].op(m1, m2)
  }
}
