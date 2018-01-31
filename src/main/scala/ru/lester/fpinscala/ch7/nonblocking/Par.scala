package ru.lester.fpinscala.ch7.nonblocking

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Par {
  sealed trait Future[+A] {
    private[nonblocking] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es){a => ref.set(a); latch.countDown()}
    latch.await()
    ref.get()
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      override private[nonblocking] def apply(cb: A => Unit): Unit = cb(a)
    }

  def delay[A](a: => A): Par[A] =
    es => new Future[A] {
      override private[nonblocking] def apply(cb: A => Unit): Unit = cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      override private[nonblocking] def apply(cb: A => Unit): Unit = {
        eval(es)(a(es)(cb))
      }
    }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {def call = r})

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    es => new Future[B] {
      override private[nonblocking] def apply(cb: B => Unit): Unit =
        pa(es) (a => eval(es) {cb(f(a))})
    }

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => new Future[C] {
      override private[nonblocking] def apply(cb: C => Unit) = {
        var ar: Option[A] = None
        var br: Option[B] = None

        val combiner = Actor[Either[A, B]] (es) {
          case Left(a) => br match {
            case None => ar = Some(a)
            case Some(b) => eval(es)(cb(f(a, b)))
          }

          case Right(b) => ar match {
            case None => br = Some(b)
            case Some(a) => eval(es)(cb(f(a, b)))
          }
        }

        pa(es)(a => combiner ! Left(a))
        pb(es)(b => combiner ! Right(b))
      }
    }


  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def mapM2[A, B](par: Par[A])(f: A => B): Par[B] = map2(par, unit(()))((a, _) => f(a))

  /*
  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val lp: List[Par[B]] = ps.map(asyncF(f))
    sequence(lp)
  }

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    pas.foldRight[Par[List[A]]](unit(Nil))((pa, pacc) => map2(pa, pacc)((a, acc) => a :: acc))
  */


  def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
    es => new Future[A] {
      override private[nonblocking] def apply(cb: A => Unit): Unit =
        pn(es) (i => eval(es) {choices(i)(es)(cb)})
    }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    choiceN(mapM2(cond)(if(_) 1 else 0))(List(f, t))

  def choiceMap[K, V](pkey: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    es => new Future[V] {
      override private[nonblocking] def apply(cb: V => Unit): Unit =
        pkey(es) (k => eval(es) {choices(k)(es)(cb)})
    }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    es => new Future[B] {
      override private[nonblocking] def apply(cb: B => Unit): Unit =
        pa(es)(a => eval(es) {f(a)(es)(cb)})
    }

  def choiceCF[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    flatMap(cond)(b => if(b) t else f)

  def choiceNF[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
    flatMap(pn)(choices(_))

  def choiceMapF[K, V](pkey: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    flatMap(pkey)(choices(_))

  def join[A](ppa: Par[Par[A]]): Par[A] =
    es => new Future[A] {
      override private[nonblocking] def apply(cb: A => Unit): Unit =
        ppa(es)(pa => eval(es){pa(es)(a => cb(a))})
    }

  def flatMapJ[A, B](pa: Par[A])(f: A => Par[B]): Par[B] =
    join(map(pa)(f))

  def joinF[A](ppa: Par[Par[A]]): Par[A] =
    flatMap(ppa)(pa => pa)

  def async[A](run: (A => Unit) => Unit): Par[A] = es => new Future[A] {
    override private[nonblocking] def apply(k: A => Unit): Unit = run(k)
  }
}
