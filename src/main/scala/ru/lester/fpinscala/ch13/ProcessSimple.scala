package ru.lester.fpinscala.ch13

import ru.lester.fpinscala.ch10.Monad
import ru.lester.fpinscala.ch11.{IO, Files, IOSimple}

sealed trait ProcessSimple[I,O] {
  import ProcessSimple._

  def apply(s: Stream[I]): Stream[O] = this match {
    case Halt() => Stream()
    case Await(recv) => s match {
      case h #:: t => recv(Some(h))(t)
      case xs      => recv(None)(xs)
    }
    case Emit(h, t) => h #:: t(s)
  }

  def repeat: ProcessSimple[I, O] = {
    def go(p: ProcessSimple[I, O]): ProcessSimple[I, O] = p match {
      case Halt() => go(this)
      case Await(recv) => Await {
        case None => Halt()
        case i    => go(recv(i))
      }
      case Emit(h, t) => Emit(h, go(t))
    }

    go(this)
  }

  def |>[O2](p2: ProcessSimple[O, O2]): ProcessSimple[I, O2] = p2 match {
    case Halt()     => Halt()
    case Emit(h, t) => Emit(h, this |> t)
    case Await(f)   => this match {
      case Emit(h, t)    => t |> f(Some(h))
      case Halt()        => Halt() |> f(None)
      case Await(g)      => Await((oi: Option[I]) => g(oi) |> p2)
    }
  }

  def map[O2](f: O => O2): ProcessSimple[I, O2] = this |> lift(f)

  def ++(p: => ProcessSimple[I, O]): ProcessSimple[I, O] = this match {
    case Halt()        => p
    case Emit(h, t)    => Emit(h, t ++ p)
    case Await(recv)   => Await(recv andThen (_ ++ p))
  }

  def flatMap[O2](f: O => ProcessSimple[I, O2]): ProcessSimple[I, O2] = this match {
    case Halt()      => Halt()
    case Emit(h, t)  => f(h) ++ t.flatMap(f)
    case Await(recv) => Await(recv andThen (_ flatMap f))
  }

  def zip[O2](p: ProcessSimple[I, O2]): ProcessSimple[I, (O, O2)] = ProcessSimple.zip(this, p)

  def zipWithIndex: ProcessSimple[I, (O, Int)] = this zip count
}

case class Emit[I, O](head: O, tail: ProcessSimple[I, O] = Halt[I, O]()) extends ProcessSimple[I, O]
case class Await[I, O](recv: Option[I] => ProcessSimple[I, O]) extends ProcessSimple[I, O]
case class Halt[I, O]() extends ProcessSimple[I, O]

object ProcessSimple {
  def emit[I, O](head: O, tail: ProcessSimple[I, O]): ProcessSimple[I, O] = Emit[I, O](head, tail)
  def await[I, O](f: I => ProcessSimple[I, O], fallback: ProcessSimple[I, O] = Halt[I, O]()): ProcessSimple[I, O] = Await[I, O] {
    case Some(i) => f(i)
    case None => fallback
  }
  def halt[I, O]: ProcessSimple[I, O] = Halt[I, O]()

  def liftOne[I, O](f: I => O): ProcessSimple[I, O] = Await[I, O] {
    case Some(i) => Emit(f(i))
    case None    => Halt()
  }

  def lift[I, O](f: I => O): ProcessSimple[I, O] = liftOne(f).repeat

  def filter[I](p: I => Boolean): ProcessSimple[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i)
    case _ => Halt()
  }.repeat

  def sum: ProcessSimple[Double, Double] = {
    def go(acc: Double): ProcessSimple[Double, Double] = Await[Double, Double] {
      case Some(d) => Emit(d + acc, go(d + acc))
      case None => Halt()
    }

    go(0.0d)
  }

  def myTake[I](n: Int): ProcessSimple[I, I] = Await[I, I] {
    case Some(i) if n > 0 => Emit(i, myTake[I](n - 1))
    case _ => Halt()
  }

  def myDrop[I](n: Int): ProcessSimple[I, I] = Await[I, I] {
    case Some(i) => if (n > 0) myDrop[I](n - 1) else Emit(i, id)
    case _ => Halt()
  }

  def id[I]: ProcessSimple[I, I] = lift(identity)

  def take[I](n: Int): ProcessSimple[I, I] = if(n <= 0) Halt() else await(i => emit(i, take[I](n - 1)))
  def drop[I](n: Int): ProcessSimple[I , I] = if(n <= 0) id else await(i => drop[I](n - 1))
  def myTakeWhile[I](p: I => Boolean): ProcessSimple[I, I] = Await[I, I] {
    case Some(i) if p(i) => Emit(i, myTakeWhile(p))
    case _ => Halt()
  }
  def myDropWhile[I](p: I => Boolean): ProcessSimple[I, I] = Await {
    case Some(i) => if(p(i)) myDropWhile(p) else Emit(i, myDropWhile(p))
    case _ => Halt()
  }

  def takeWhile[I](p: I => Boolean): ProcessSimple[I, I] = await(i => if(p(i)) emit(i, takeWhile[I](p)) else Halt())
  def dropWhile[I](p: I => Boolean): ProcessSimple[I, I] = await(i => if(p(i)) dropWhile[I](p) else emit(i, id))

  def count[I]: ProcessSimple[I, Int] = {
    def go(acc: Int): ProcessSimple[I, Int] = await(i => { val next = acc + 1; emit(next, go(next))})
    emit(0, go(0))
  }

  def mean[I]: ProcessSimple[Double ,Double] = {
    def go(acc: (Double, Double)): ProcessSimple[Double, Double] = await(i => { val next = (acc._1 + i, acc._2 + 1); emit (next._1 / next._2, go(next))})
    go((0.0d, 0.0d))
  }

  def loop[S, I, O](z: S)(f: (I, S) => (O, S)): ProcessSimple[I, O] = await(i => { val os = f(i, z); emit(os._1, loop(os._2)(f))})

  def sumLoop: ProcessSimple[Double, Double] = loop(0.0d) {
    case (i, acc) =>
      val newAcc = acc + i
      (newAcc, newAcc)
  }

  def countLoop[I]: ProcessSimple[I, Int] = loop[Int, I, Int](0) {
    case (i, acc) =>
      val newAcc = acc + 1
      (newAcc, newAcc)
  }

  implicit def processMonad[I]: Monad[({type l[X] = ProcessSimple[I, X]})#l] = new Monad[({type l[X] = ProcessSimple[I, X]})#l] {
    override def unit[O](o: => O): ProcessSimple[I, O] = Emit(o)
    override def flatMap[O, O2](p: ProcessSimple[I, O])(f: O => ProcessSimple[I, O2]): ProcessSimple[I, O2] = p flatMap f
  }

  def zip[A, B, C](p1: ProcessSimple[A, B], p2: ProcessSimple[A, C]): ProcessSimple[A, (B, C)] = (p1, p2) match {
    case (Halt(), _)             => Halt()
    case (_, Halt())             => Halt()
    case (Emit(h1, t1), Emit(h2, t2)) => Emit((h1, h2), zip(t1, t2))
    //case (Await(recv1), Await(recv2)) => Await(oi => recv1(oi) flatMap (o => recv2(oi) map (o2 => (o, o2))))
    case (Await(recv1), _)      => Await((oa: Option[A]) => zip(recv1(oa), feed(oa)(p2)))
    case (_, Await(recv2))      => Await((oa: Option[A]) => zip(feed(oa)(p1), recv2(oa)))
  }

  def feed[A, B](oa: Option[A])(p: ProcessSimple[A, B]): ProcessSimple[A, B] = p match {
    case Halt() => p
    case Emit(h, t) => Emit(h, feed(oa)(t))
    case Await(recv) => recv(oa)
  }

  /*
  def myZipWithIndex[I, O]: ProcessSimple[I, (I, Int)] = loop[Int, I, (I, Int)] (0) {
    case (i, acc) => {
      val newAcc = acc + 1
      ((i, newAcc), newAcc)
    }
  }*/


  def mean2[I]: ProcessSimple[Double ,Double] = sum.zipWithIndex map {
    case (d, i) => d / (i + 1).toDouble
  }

  def exists[I](f: I => Boolean): ProcessSimple[I, Boolean] = await(i => emit(f(i), exists(f)))

  def processFile[A, B](f: java.io.File, p: ProcessSimple[String, A], z: B)(g: (B, A) => B): IO[B] = IO {
    def go(ss: Iterator[String], cur: ProcessSimple[String, A], acc: B): B =
      cur match {
        case Halt()      => acc
        case Await(recv) =>
          val next = if(ss.hasNext) recv(Some(ss.next)) else recv(None)
          go(ss, next, acc)
        case Emit(h, t)  => go(ss, t, g(acc, h))
      }
    val s = io.Source.fromFile(f)
    try go(s.getLines, p, z)
    finally s.close
  }

  val p: ProcessSimple[String, String] = filter((line: String) => !line.startsWith("#")) |> filter((line: String) => line.trim.nonEmpty) |> lift(s => Files.fahrenheitToCelsius(s.toDouble).toString)

  def convertToCelsius(in: java.io.File, out: java.io.File): Unit = processFile(in,  p, List.empty[String])((acc, c) => c :: acc).flatMap( ls => IO { /*TODO: write result to a file*/ })
}