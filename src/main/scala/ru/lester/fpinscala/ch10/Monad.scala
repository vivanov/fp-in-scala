package ru.lester.fpinscala.ch10

import ru.lester.fpinscala.ch11.{Free, Return, Suspend}
import ru.lester.fpinscala.ch6.State
import ru.lester.fpinscala.ch7.nonblocking.Par
import ru.lester.fpinscala.ch7.nonblocking.Par.Par
import ru.lester.fpinscala.ch7.nonblocking.Par.Par
import ru.lester.fpinscala.ch8.Gen

trait Monad[M[_]] extends Applicative[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = join(map(ma)(f))
  def join[A](mma: M[M[A]]): M[A] = flatMap(mma)(identity)

  override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => unit(f(a)))
  override def map2[A, B, C](ma: M[A], mb: M[B])(f: (A,B) => C): M[C] = flatMap(ma)(a => map(mb)(b => f(a, b)))

  //def sequence[A](lma: List[M[A]]): M[List[A]] = traverse(lma)(identity)
  //def traverse[A, B](lma: List[A])(f: A => M[B]): M[List[B]] = lma.foldRight(unit(Nil: List[B]))((a, macc) => flatMap(f(a))(b => map(macc)(acc => b :: acc)))
  //def replicateM[A](n: Int, ma: M[A]): M[List[A]] = sequence(List.fill(n)(ma))
  def filterM[A](la: List[A])(f: A => M[Boolean]): M[List[A]] = la.foldRight(unit(Nil: List[A]))((a, macc) => flatMap(f(a))(bool => map(macc)(acc => if(bool) a :: acc else acc)))

  def as[A, B](ma: M[A])(b: B): M[B] = ma map (_ => b)
  def skip[A](ma: M[A]): M[Unit] = as(ma)(())
  def when[A](b: Boolean)(ma: => M[A]): M[Boolean] = if(b) as(ma)(true) else unit(false)

  def doWhile[A](ma: M[A])(f: A => M[Boolean]): M[Unit] = for {
    a1 <- ma
    b <- f(a1)
    _ <- if(b) doWhile(ma)(f) else unit(())
  } yield ()

  def forever[A, B](ma: M[A]): M[B] = {
    lazy val t: M[B] = forever(ma)
    ma flatMap (_ => t)
  }

  def foldM[A, B](l: Stream[A])(z: B)(f: (B, A) => M[B]): M[B] = l match {
    case h #:: t => f(z, h) flatMap(z1 => foldM(t)(z1)(f))
    case _ => unit(z)
  }

  def foldM_[A, B](l: Stream[A])(z: B)(f: (B, A) => M[B]): M[Unit] = skip { foldM(l)(z)(f) }

  def foreachM[A](l: Stream[A])(f: A => M[Unit]): M[Unit] = foldM_(l)(())((u, a) => skip(f(a)))

  implicit def toMonadic[A](ma: M[A]): Monadic[M, A] = new Monadic[M, A] {
    val M = Monad.this; def get = ma
  }
}

trait Monadic[M[_], A] {
  val M: Monad[M]
  import M._
  def get: M[A]
  private val ma = get
  def map[B](f: A => B): M[B] = M.map(ma)(f)
  def flatMap[B](f: A => M[B]): M[B] = M.flatMap(ma)(f)
  def **[B](mb: M[B]) = M.map2(ma, mb)((_, _))
  def >*[B](mb: M[B]) = M.map2(ma, mb)((a, b) => b)
  def as[B](b: B) = M.as(ma)(b)
  def skip: M[Unit] = M.skip(ma)
}

object Monad {
  // Added implicit modifier for Traverse example
  implicit val idMonad = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = Id(a)
    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma flatMap f
  }

  val genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A, B](ma: Gen[A])(f: A => Gen[B]): Gen[B] = ma flatMap f
  }

  val listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma flatMap f
  }

  implicit val optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = ma flatMap f
  }

  /*
  val intStateMonad = new Monad[({type l[A] =  State[Int, A]})#l] {
    override def unit[A](a: => A): State[Int, A] = State.unit(a)
    override def flatMap[A, B](ma: State[Int, A])(f: A => State[Int, B]): State[Int, B] = ma flatMap f
  }

  val stringStateMonad = new Monad[({type l[A] =  State[String, A]})#l] {
    override def unit[A](a: => A): State[String, A] = State.unit(a)
    override def flatMap[A, B](ma: State[String, A])(f: A => State[String, B]): State[String, B] = ma flatMap f
  }
  */

  implicit def stateMonad[S] = new Monad[({type l[A] =  State[S, A]})#l] {
    override def unit[A](a: => A): State[S, A] = State.unit(a)
    override def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = ma flatMap f
  }

  def eitherMonad[E] = new Monad[({type l[X] = Either[E, X]})#l] {
    override def unit[A](a: => A): Either[E, A] = Right(a)
    override def flatMap[A, B](ea: Either[E, A])(f: A => Either[E, B]): Either[E, B] = ea match {
      case Left(e)  => Left(e)
      case Right(a) => f(a)
    }
  }

  implicit val function0Monad = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](fa: Function0[A])(f: A => Function0[B]): Function0[B] = () => f(fa())()
  }

  implicit val parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = Par.fork { Par.flatMap(pa)(f) }
  }

  def freeMonad[F[_]] = new Monad[({type l[A]= Free[F, A]})#l] {
    override def unit[A](a: => A): Free[F, A] = Return(a)
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] = fa.flatMap(f)
  }
}