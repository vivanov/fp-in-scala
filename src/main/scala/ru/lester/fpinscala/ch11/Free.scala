package ru.lester.fpinscala.ch11

import ru.lester.fpinscala.ch10.Monad
import ru.lester.fpinscala.ch7.nonblocking.Par
import Par._
import Monad._

sealed trait Free[F[_], A] {
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
  def map[B](f: A => B): Free[F, B] = flatMap(f andThen (Return(_)))
}

case class Return[F[_], A](a: A) extends Free[F, A]
case class Suspend[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap[F[_], A, B](sub: Free[F, A], k: A => Free[F, B]) extends Free[F, B]


object Free {

  type TailRec[A] = Free[Function0, A]
  type Async[A]   = Free[Par, A]

  /*
  @annotation.tailrec
  def step[A](async: Async[A]): Async[A] = async match {
    case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
    case FlatMap(Return(x), f)     => step(f(x))
    case _                         => async
  }

  def runAsync[A](async: Async[A]): Par[A] = step(async) match {
    case Return(a)     => Par.unit(a)
    case Suspend(r)    => r
    case FlatMap(x, f) => x match {
      case Suspend(r)    => Par.flatMap(r)(a => runAsync(f(a)))
      case _             => sys.error("Impossible, since `step` eliminates these cases")
    }
  }
  */

  @annotation.tailrec
  def runTailRec[A](tramp: TailRec[A]): A = tramp match {
    case Return(a)       => a
    case Suspend(r)      => r()
    case FlatMap(x, f)   => x match {
      case Return(a)     => runTailRec(f(a))
      case Suspend(r)    => runTailRec(f(r()))
      case FlatMap(y, g) => runTailRec(y flatMap (a => g(a) flatMap f))
    }
  }

  def run[F[_], A](free: Free[F, A])(implicit ev: Monad[F]): F[A] = {
    @annotation.tailrec
    def step(fa: Free[F, A]): Free[F, A] = fa match {
      case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
      case FlatMap(Return(x), f)     => step(f(x))
      case _                         => fa
    }

    step(free) match {
      case Return(a)  => ev.unit(a)
      case Suspend(r) => r
      case FlatMap(x, f) => x match {
        case Suspend(r) => ev.flatMap(r)(a => run(f(a)))
        case _ => sys.error("Impossible, since `step` eliminates these cases")
      }
    }
  }

  trait Translate[F[_], G[_]] {
    def apply[A](fa: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def runFree[F[_], G[_], A](free: Free[F, A])(t: F ~> G)(implicit ev: Monad[G]): G[A] = {
    @annotation.tailrec
    def step(fa: Free[F, A]): Free[F, A] = fa match {
      case FlatMap(FlatMap(x, f), g) => step(x.flatMap(a => f(a).flatMap(g)))
      case FlatMap(Return(x), f)     => step(f(x))
      case _                         => fa
    }

    step(free) match {
      case Return(a)  => ev.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r), f) => ev.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }
  }


  def translate[F[_], G[_], A](f: Free[F, A])(t: F ~> G): Free[G, A] = {
    type FreeG[X] = Free[G, X]
    def toFreeG = new (F ~> FreeG) {
      override def apply[T](fa: F[T]): FreeG[T] = Suspend(t(fa))
    }

    runFree[F, FreeG, A](f)(toFreeG)(freeMonad[G])
  }
}