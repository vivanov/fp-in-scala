package ru.lester.fpinscala.ch7

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone = true
    override def get(timeout: Long, units: TimeUnit) = get
    override def cancel(evenIfRunning: Boolean) = false
    override def isCancelled = false
  }

  private case class Map2Future[A, B, C](fa: Future[A], fb: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    override def isDone = cache.isDefined
    override def isCancelled = fa.isCancelled || fb.isCancelled
    override def cancel(evenIfRunning: Boolean) = fa.cancel(evenIfRunning) || fb.cancel(evenIfRunning)
    override def get: C = compute(Long.MaxValue)
    override def get(timeout: Long, units: TimeUnit): C = compute(TimeUnit.MILLISECONDS.convert(timeout, units))


    def compute(timeoutInMillis: Long): C = cache match {
      case Some(c) => c
      case None => {
        val beforeFA = System.currentTimeMillis
        val a = fa.get(timeoutInMillis, TimeUnit.MILLISECONDS)
        val afterFA = System.currentTimeMillis
        val at = afterFA - beforeFA
        val b = fb.get(timeoutInMillis - at, TimeUnit.MILLISECONDS)
        val res = f(a, b)
        cache = Some(res)
        res
      }
    }
  }

  def map2[A, B, C](pa: => Par[A], pb: => Par[B]) (f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val fa = pa(es)
      val fb = pb(es)
      UnitFuture(f(fa.get, fb.get))
    }
  }

  def map2Timeout[A, B, C](pa: => Par[A], pb: => Par[B]) (f: (A, B) => C): Par[C] = {
    (es: ExecutorService) => {
      val (fa, fb) = (pa(es), pb(es))
      Map2Future(fa, fb, f)
    }
  }


  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      override def call() = a(es).get
    })

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService) (a: Par[A]): Future[A] = a(s)

  def asyncF[A, B](f: A => B): A => Par[B] = (a: A) => lazyUnit(f(a))

  def map[A, B](par: Par[A])(f: A => B): Par[B] = map2(par, unit(()))((a, _) => f(a))

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val lp: List[Par[B]] = ps.map(asyncF(f))
    sequence(lp)
  }

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    pas.foldRight[Par[List[A]]](unit(Nil))((pa, pacc) => map2(pa, pacc)((a, acc) => a :: acc))


  def parFilter[A](ps: List[A])(f: A => Boolean): Par[List[A]] = {
    val fa = asyncF(f)
    ps.foldRight[Par[List[A]]](unit(Nil))((a, pacc) => {
      map2(fa(a), pacc)((res, acc) => if(res) a :: acc else acc)
    })
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if(ints.length <= 1) unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(sum(l)), fork(sum(r))) (_ + _)
    }
  }

  def op(ints: IndexedSeq[Int])(f: (Int, Int) => Int): Par[Int] = {
    if(ints.length <= 1) unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      map2(fork(op(l)(f)), fork(op(r)(f))) (f)
    }
  }

  def sumop(ints: IndexedSeq[Int]): Par[Int] = op(ints) (_ + _)
  def maxop(ints: IndexedSeq[Int]): Par[Int] = op(ints) (scala.math.max)

  def countWords(ls: List[String]): Par[Int] =
    map(parMap(ls)(_.split(" ").length))(_.foldLeft(0)(_ + _))

  def map3[A, B, C, D](pa: => Par[A], pb: => Par[B], pc: Par[C])(f: (A, B, C) => D): Par[D] = {
    val pab: Par[C => D] = map2(pa, pb) ((a, b) => f.curried (a) (b))
    map2(pc, pab) ((c, fab) => fab(c))
  }

  def map4[A, B, C, D, E](pa: => Par[A], pb: => Par[B], pc: Par[C], pd: => Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val pab: Par[C => D => E] = map2(pa, pb)((a, b) => f.curried (a) (b))
    val pabc: Par[D => E] = map2(pc, pab)((c, fabc) => fabc(c))
    map2(pd, pabc)((d, fabcd) => fabcd(d))
  }

  def map5[A, B, C, D, E, F](pa: => Par[A], pb: => Par[B], pc: Par[C], pd: => Par[D], pe: => Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    val pab: Par[C => D => E => F] = map2(pa, pb)((a, b) => f.curried (a) (b))
    val pabc: Par[D => E => F] = map2(pc, pab)((c, fabc) => fabc(c))
    val pabcd: Par[E => F] = map2(pd, pabc)((d, fabcd) => fabcd(d))
    map2(pe, pabcd)((e, fabcde) => fabcde(e))
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)
}