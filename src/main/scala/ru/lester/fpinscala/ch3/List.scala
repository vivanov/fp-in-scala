package ru.lester.fpinscala.ch3

/**
 * Created with IntelliJ IDEA.
 * User: lester
 * Date: 06.10.14
 * Time: 0:20
 */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = {
    if(as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def sum(l: List[Int]): Int = l match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(l: List[Double]): Double = l match {
    case Nil => 1.0
    case Cons(0.0, xs) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], newHead: A): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(newHead, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case lst @ Cons(_, _) if (n <= 0) => lst
      case Cons(_, xs) => drop(xs, n - 1)
    }
  }

  def dropWhile[A](as: List[A]) (f: A => Boolean): List[A] = {
    as match {
      case Cons(h, t) if(f(h)) => dropWhile(t) (f)
      case _ => as
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(x, xs) => Cons(x, append(xs, a2))
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(x2, (Cons(x1, Nil))) => Cons(x2, Nil)
      case Cons(x, xs) => Cons(x, init(xs))
    }
  }

  // Consumes stack frames
  def foldRight[A, B] (as: List[A], z: B) (f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z) (f))
    }
  }

  def sumr(l: List[Int]): Int = foldRight(l, 0) ((el, acc) => el + acc)
  def productr(l: List[Double]): Double = foldRight(l, 1.0) (_ * _)
  //Won't work! foldRight have to traverse all list before collapsing (applying function)!
  //def productr(l: List[Double]): Double = foldRight(l, 1.0) ((num, acc) => if(num == 0.0) 0.0 else {num * acc})

  def lengthr[A](as: List[A]) = foldRight(as, 0) ((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A, B] (as: List[A], z: B) (f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x)) (f)
    }
  }

  def suml(l: List[Int]): Int = foldLeft(l, 0) ((acc, el) => acc + el)
  def productl(l: List[Double]): Double = foldLeft(l, 0.0) ((el, acc) => el * acc)
  def lengthl[A](l: List[A]): Int = foldLeft(l, 0) ((acc, _) => acc + 1)

  def reversef[A](as: List[A]) = foldLeft(as, Nil: List[A]) ((ys, x) => Cons(x, ys))
  def appendf[A](xs: List[A], ys: List[A]) = foldRight(xs, ys) ((x, ys) => Cons(x, ys))

  def concat[A](as: List[List[A]]): List[A] = foldRight(as, Nil: List[A]) (appendf)

  def addOne(as: List[Int]): List[Int] = foldRight(as, Nil: List[Int]) ((x, ys) => Cons(x + 1, ys))
  def doubleToString(as: List[Double]): List[String] = foldRight(as, Nil: List[String]) ((x, ys) => Cons(x.toString, ys))

  // This implementation is not stack-safe due to foldRight usage
  def map[A, B](as: List[A]) (f: A => B): List[B] = foldRight(as, Nil: List[B]) ((x, ys) => Cons(f(x), ys))
  // Stack-safe, local mutation, not observable outside function
  def map2[A, B](as: List[A]) (f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]): Unit = l match {
      case Nil => ()
      case Cons(x, xs) => buf += f(x); go(xs)
    }
    go(as)
    List(buf.toList: _*)
  }

  def filter[A](as: List[A]) (f: A => Boolean): List[A] = foldRight(as, Nil: List[A]) ((x, ys) => if(f(x)) Cons(x, ys) else ys)
  def excludeOdds(as: List[Int]) = filter(as) (_ % 2 == 0)

  def flatMap[A, B](as: List[A]) (f: A => List[B]): List[B] = concat(map(as) (f))
  def filterf[A](as: List[A]) (f: A => Boolean): List[A] = flatMap(as) (x => if(f(x)) List(x) else Nil)

  def zipIntsWithAdd(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match {
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipIntsWithAdd(xs, ys))
    case (_, Nil) => Nil
    case (Nil, _) => Nil
  }

  def zipWith[A, B, C](l1: List[A], l2: List[B]) (f: (A, B) => C) : List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Cons(x, xs), l@Cons(y, ys)) => if(x == y) hasSubsequence(xs, ys) else hasSubsequence(xs, l)
    case (_, Nil) => true
    case (Nil, _) => false
  }
}
