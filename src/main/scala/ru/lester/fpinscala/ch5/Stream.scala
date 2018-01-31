package ru.lester.fpinscala.ch5

import Stream._
/**
 * User: vvivanov
 * Date: 12.11.2014
 * Time: 19:00
 */

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // The natural recursive solution
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }


  /*
  The above solution will stack overflow for large streams, since it's
  not tail-recursive. Here is a tail-recursive implementation. At each
  step we cons onto the front of the `acc` list, which will result in the
  reverse of the stream. Then at the end we reverse the result to get the
  correct order again.
  */
  def toListTailRec: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Empty => l
      case Cons(h, t) => go(t(), h() :: l)
    }
    go(this, List()).reverse
  }

  /*
  In order to avoid the `reverse` at the end, we could write it using a
  mutable list buffer and an explicit loop instead. Note that the mutable
  list buffer never escapes our `toList` method, so this function is
  still _pure_.
  */

  def toListFast: List[A] = {
    val buf =  new scala.collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Empty => buf.toList
      case Cons(h, t) => {
        buf += h()
        go(t())
      }
    }
    go(this)
  }

  def take(n: Int): Stream[A] = {
    if(n > 0) this match {
      case Cons(h, t) if n == 1 => cons(h(), empty)
      case Cons(h, t) => cons(h(), t().take(n - 1))
      case _ => empty
    }
    else Stream()
  }


  def drop(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], n: Int): Stream[A] = {
      if(n <= 0) s
      else s match {
        case Cons(_, t) => go(t(), n - 1)
        case _ => Stream()
      }
    }
    go(this, n)
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) ||  t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def existsF(p: A => Boolean): Boolean = foldRight(false) ((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true) ((a, b) => p(a) && b)

  def takeWhileF(f: A => Boolean): Stream[A] = foldRight(empty[A]) ((a, b) => if(f(a)) cons(a, b) else empty)

  def headOptionF: Option[A] = foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] = foldRight(empty: Stream[B]) ((a, b) => cons(f(a), b))
  def filter(f: A => Boolean): Stream[A] = foldRight(empty: Stream[A]) ((a, b) => if(f(a)) cons(a, b) else b)
  def append[B >: A](s: => Stream[B]): Stream[B] = foldRight(s) ((a, b) => cons(a, b))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty: Stream[B]) ((a, b) => f(a) append b)

  def find(p: A => Boolean): Option[A] = filter(p).headOptionF

  def mapUF[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
  }

  def takeUF(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), m) if m > 0 => Some((h(), (t(), m - 1)))
    case _ => None
  }

  def takeWhileUF(f: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if f(h()) => Some((h(), t()))
    case _ => None
  }

  def zipUF[B](s2: Stream[B]): Stream[(A, B)] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
    case (_, Empty) => None
    case (Empty, _) => None
  }

  def zipWithUF[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
    case (_, Empty) => None
    case (Empty, _) => None
  }

  def zipAllUF[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
    case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
    case (Empty, Empty) => None
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = this.zipWithUF(s){_ == _}.forAll(_ == true)

  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(h, t) => Some(s, t())
    case Empty => None
  } append Stream.empty

  def hasSubsequence[B >: A](s: Stream[B]) = tails exists (_ startsWith s)

  // Can't use unfold because it scans from the beginning
  /*
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = unfold(this) {
    case s@Cons(h, t) => Some(s.foldRight(z: B){(a, b) => f(a, b)}, t())
    case Empty => None
  } append Stream.empty
  */

  // My implementation with foldRight
  //def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight(cons(z, empty)) {(a, b) => cons(f(a, b match {case Cons(h, t) => h(); case Empty => z}), b)}

  // Authors implementation with foldRight
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, cons(z, empty))) {(a, b) => {
      val r = f(a, b._1)
      (r, cons(r, b._2))
    }
  }._2
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]



object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def go(cur: Int, next: Int): Stream[Int] = {
      cons(cur, go(next, cur + next))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a,s)) => cons(a, unfold(s)(f))
      case _ => empty
    }
  }

  def constantuf[A](a: A): Stream[A] = unfold(a)(a1 => Some((a1, a1)))
  def fromuf(n: Int): Stream[Int] = unfold(n)(n1 => Some(n1, n1 + 1))
  def fibsuf: Stream[Int] = unfold[Int, (Int, Int)]((0,1)){ case (cur: Int, next: Int) => Some((cur, (next, cur + next))) }
}
