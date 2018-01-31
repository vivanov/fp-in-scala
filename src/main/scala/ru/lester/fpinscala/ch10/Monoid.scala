package ru.lester.fpinscala.ch10

import ru.lester.fpinscala.ch3.List
import ru.lester.fpinscala.ch8.{Gen, Prop}
import Gen._
import Prop._
import Foldable._

trait Monoid[A] {
  def op(a1: A, a2: A) : A // satisfies op(x, op(y,z)) == op(op(x, y), z)
  def zero: A              // satisfies op(zero, x) == op(x, zero)
}

object Monoid {
  implicit val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String): String = a1 + a2
    override def zero: String = ""
  }

  implicit def intAddMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  implicit def intMultMonoid = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  implicit def orMonoid = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  implicit def andMonoid = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  implicit def listMonoid[A] = new Monoid[List[A]] {
    override def op(a1: List[A], a2: List[A]): List[A] = List.append(a1, a2)
    override def zero: List[A] = ru.lester.fpinscala.ch3.Nil
  }

  implicit def vectorMonoid[A] = new Monoid[Vector[A]] {
    override def op(a1: Vector[A], a2: Vector[A]): Vector[A] = a1 ++ a2
    override def zero: Vector[A] = Vector.empty[A]
  }

  /*
  def optionMonoid[A: Monoid] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = for {
      aa1 <- a1
      aa2 <- a2
    } yield implicitly[Monoid[A]].op(aa1, aa2)
    override def zero: Option[A] = Some(implicitly[Monoid[A]].zero)
  }
  */

  implicit def optionMonoid[A](implicit monoidA: Monoid[A]) = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = for {
      aa1 <- a1
      aa2 <- a2
    } yield monoidA.op(aa1, aa2)
    override def zero: Option[A] = Some(monoidA.zero)
  }


  implicit def endoMonoid[A] = new Monoid[A => A] {
    override def op(f1: A => A, f2: A => A): A => A = a => f2(f1(a))
    override def zero: A => A = identity
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = forAll(gen)(a => {
    m.op(m.zero, a) == m.op(a, m.zero)
    m.op(a, m.op(a, a)) == m.op(m.op(a, a), a)
  })

  def concatenate[A](l: List[A], m: Monoid[A]): A = List.foldLeft(l, m.zero)(m.op)

  def foldMap[A, B](l: List[A], m: Monoid[B])(f: A => B): B = List.foldLeft(l, m.zero)((b, a) => m.op(b, f(a)))

  //TODO:
  /*
  def foldLeft[A, B: Monoid](lst: List[A], zero: B)(f: (B, A) => B): B = {
    def go(acc: B, l: List[A]): B = l match {
      case h :: t => go(foldMap(t, implicitly[Monoid[B]])(f.curried(acc)), t)
      case Nil => zero
    }
    go(zero, lst)
  }
  */

  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    v.splitAt(v.length / 2) match {
      case (h1 +: t1, h2 +: t2) => m.op(if(t1.isEmpty) m.op(m.zero, f(h1)) else m.op(f(h1), foldMapV(t1, m)(f)), if(t2.isEmpty) m.op(m.zero, f(h2)) else m.op(f(h2), foldMapV(t2, m)(f)))
    }
  }

  implicit def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def zero: (A, B) = (A.zero, B.zero)
    override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
  }

  implicit def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def op(a: Map[K, V], b: Map[K, V]): Map[K, V] = (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
      acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }
    override def zero: Map[K, V] = Map.empty[K,V]
  }

  val M: Monoid[Map[String, Map[String, Int]]] = mapMergeMonoid(mapMergeMonoid(intAddMonoid))

  implicit def functionMonoid[A, B](implicit ev: Monoid[B]): Monoid[A => B] = new Monoid[A => B] {
    override def op(fa: A => B, fb: A => B): A => B = a => ev.op(fa(a), fb(a))
    override def zero: A => B = _ => ev.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] = iSeqFoldable.foldMap[A, Map[A, Int]](as) (a => Map(a -> 1)) (mapMergeMonoid(intAddMonoid))
}
