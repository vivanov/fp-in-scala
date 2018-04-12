package ru.lester.fpinscala.ch10

import Applicative.monoidApplicative
import Monad.idMonad
import ru.lester.fpinscala.ch6.State
import ru.lester.fpinscala.ch6.State._

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  def traverse[G[_]: Applicative, A, B](fa: F[A])(f: A => G[B]): G[F[B]] = sequence(map(fa)(f))
  def sequence[G[_]: Applicative, A](fa: F[G[A]]): G[F[A]] = traverse(fa)(identity)

  override def map[A,B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(a => Id(f(a)))(idMonad).value
  override def foldMap[A, B: Monoid](as: F[A])(f: A => B): B = traverse[({type l[X] = Const[B, X]})#l, A, B](as)(f)(monoidApplicative)

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] = traverse[({type l[X] = State[S, X]})#l, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)(a => for {
      s1 <- get[S]
      (b, s2)  = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)
  def toList[A](fa: F[A]): List[A] = mapAccum(fa, List.empty[A])((a, s) => ((), a :: s))._2.reverse
  def reverse[A](fa: F[A]): F[A] = mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1
  override def foldLeft[A, B](fa: F[A])(z: B)(f: (B, A) => B): B = mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def fuse[G[_]: Applicative, H[_]: Applicative, A, B](fa: F[A])(f: A => G[B], g: A => H[B]): (G[F[B]], H[F[B]]) = {
    implicit val productApplicative = implicitly[Applicative[G]].productap(implicitly[Applicative[H]])
    traverse[({type l[X] = (G[X], H[X])})#l, A, B](fa)(a => (f(a), g(a)))
  }

  def compose[G[_]: Traverse]: Traverse[({type l[X] = F[G[X]]})#l] = {
    val self = this
    new Traverse[({type l[X] = F[G[X]]})#l] {
      override def traverse[H[_]: Applicative, A, B](fga: F[G[A]])(f: A => H[B]): H[F[G[B]]] = self.traverse(fga)(ga => implicitly[Traverse[G]].traverse(ga)(f))
    }
  }

  def composeM[F[_], G[_]](FM: Monad[F], GM: Monad[G], T: Traverse[G]) = new Monad[({type l[X] = F[G[X]]})#l] {
    override def unit[A](a: => A): F[G[A]] = FM.unit(GM.unit(a))
    override def flatMap[A, B](ma: F[G[A]])(f: A => F[G[B]]): F[G[B]] = FM.flatMap[G[A], G[B]](ma)((ga: G[A]) => FM.map[G[G[B]], G[B]](T.traverse[F, A, G[B]](ga)(a => f(a))(FM))(ggb => GM.join[B](ggb)))
  }
}

object Traverse {
  case class Tree[A](head: A, tail: List[Tree[A]])

  implicit val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit ev: Applicative[G]): G[List[B]] = fa.foldRight(ev.unit(List.empty[B]))((a, gacc) => ev.map2(f(a), gacc)((b, acc) => b :: acc))
  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Option[B]] = fa match {
      case Some(a) => ev.map2(f(a), ev.unit((b: B) => Some(b)))((b, s) => s(b))
      case None => ev.unit(None)
    }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit ev: Applicative[G]): G[Tree[B]] = {
      //ev.map2(f(fa.head), fa.tail.foldRight(ev.unit(List.empty[Tree[B]]))((t, tacc) => ev.map2(traverse(t)(f)(ev), tacc)((tt, acc) => tt :: acc)))((h: B, t: List[Tree[B]]) => Tree(h, t))
      ev.map2(f(fa.head), listTraverse.traverse(fa.tail)(t => traverse(t)(f)))(Tree(_, _))
    }
  }

  class ToTraverseOps[F[_]: Traverse, A](fa: F[A]) {
    def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]] = implicitly[Traverse[F]].traverse[G, A, B](fa)(f)
    def fuse[G[_]: Applicative, H[_]: Applicative, B](f: A => G[B], g: A => H[B]): (G[F[B]], H[F[B]]) = implicitly[Traverse[F]].fuse(fa)(f, g)
  }
  implicit def toTraverseOps[F[_]: Traverse, A](fa: F[A]) = new ToTraverseOps[F, A](fa)


  /*
  // Example usage of Traverse.fuse

  val list = List(1, 10, 100)
  val res = list.fuse(n => (Some(n): Option[Int]), n => Id(n))
  //res === (Some(List(1, 10, 100)),Id(List(1, 10, 100)))
  */

  /*
  // Example usage of Traverse.compose
  implicit val listoptionTraverse = listTraverse compose optionTraverse

  type ListOpt[X] = List[Option[X]]
  val listopt: ListOpt[Int] = List(Some(1), Some(10), Some(100))
  val res = listopt.traverse(n => Id(n + 1))
  // res === List(Some(2), Some(11), Some(101))

  val listopt: List[Option[Int]] = List(Some(1), Some(10), Some(100))
  val res = toTraverseOps[({type l[X] = List[Option[X]]})#l, Int](listopt).traverse(n => Id(n + 1))
  // res === List(Some(2), Some(11), Some(101))
  */
}
