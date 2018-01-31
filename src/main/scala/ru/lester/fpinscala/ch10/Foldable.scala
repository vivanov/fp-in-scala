package ru.lester.fpinscala.ch10

import ru.lester.fpinscala.ch3._
import Tree._


trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B = ???  //TODO: Implement via foldMap
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B = ???   //TODO: Implement via foldMap
  def foldMap[A, B: Monoid](as: F[A])(f: A => B): B
  def concatenate[A](as: F[A])(implicit ev: Monoid[A]) = foldLeft(as)(ev.zero) (ev.op)
}

object Foldable {
  implicit val listFoldable: Foldable[List] = new Foldable[List] {
    override def foldMap[A, B: Monoid](as: List[A])(f: (A) => B): B = ???
  }

  implicit val iSeqFoldable: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldMap[A, B: Monoid](as: IndexedSeq[A])(f: (A) => B): B = ???
  }

  implicit val treeFoldable = new Foldable[Tree] {

    override def foldMap[A, B](as: Tree[A])(f: A => B)(implicit ev: Monoid[B]): B = {
      def go(tree: Tree[A]): B = tree match {
        case Leaf(v)      => ev.op(f(v), ev.zero)
        case Branch(l, r) => ev.op(go(l), go(r))
      }
      go(as)
    }
  }


  implicit val optionFoldable = new Foldable[Option] {
    override def foldMap[A, B: Monoid](as: Option[A])(f: A => B): B = {
      val bMonoid: Monoid[B] = implicitly[Monoid[B]]
      as match {
        case None    => bMonoid.zero
        case Some(a) => bMonoid.op(f(a), bMonoid.zero)
      }
    }
  }

  def toList[F[_]: Foldable, A](as: F[A]): List[A] = implicitly[Foldable[F]].foldMap(as)(List(_))

}