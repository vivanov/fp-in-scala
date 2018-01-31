package ru.lester.fpinscala.ch3

/**
 * Created with IntelliJ IDEA.
 * User: lester
 * Date: 17.10.14
 * Time: 3:37
 */
object Tree {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + 1 + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => {
      val lmax = maximum(l)
      val rmax = maximum(r)
      lmax max rmax
    }
  }

  /*
  def depth[A](tree: Tree[A]): Int = {
    def go(level: Int, tree: Tree[A]): Int = tree match {
      case Leaf(_) => level
      case Branch(l, r) => {
        val dl = go(level + 1, l)
        val dr = go(level + 1, r)
        dl max dr
      }

    }
    go(0, tree)
  }
  */

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A]) (f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B) (g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => {
      val lz = fold(l)(f)(g)
      val rz = fold(r)(f)(g)
      g(lz, rz)
    }
  }

  def sizef[A](tree: Tree[A]): Int = fold(tree)(v => 1)(_ + 1 + _)
  def maximumf(tree: Tree[Int]): Int = fold(tree)(v => v)(_ max _)
  def depthf[A](tree: Tree[A]): Int = fold(tree)(v => 0) ((dl, dr) => 1 + (dl max dr))

  //Use helper functions that call corresponding constructors, but return less specific (more general) type
  //in order to avoid to explicit type annotation and limitation of Scala encoding of ADT via subtyping:

  def leaf[A](a: A): Tree[A] = Leaf(a)
  def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)


  //def mapf[A, B](tree: Tree[A]) (f: A => B): Tree[B] = fold(tree) (v => Leaf(f(v)): Tree[B]) (Branch(_, _))
  def mapf[A, B](tree: Tree[A]) (f: A => B): Tree[B] = fold(tree) (v => leaf(f(v))) (branch)
}
