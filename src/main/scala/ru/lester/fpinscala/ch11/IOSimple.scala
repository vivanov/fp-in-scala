package ru.lester.fpinscala.ch11

import ru.lester.fpinscala.ch10.Monad

sealed trait IOSimple[A] { self =>
  //def run(): A
  //def map[B](f: A => B): IO[B] = new IO[B] { def run() = f(self.run()) }
  //def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] {def run() = f(self.run()).run()}

  /*
  def ++(io: IO) = new IO {
    def run() = { self.run(); io.run() }
  }
  */

  //def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(self, f)
  //def map[B](f: A => B): IO[B] = flatMap(f andThen (Return(_)))
}

/*
case class Return[A](a: A) extends IO[A]
case class Suspend[A](a: () => A) extends IO[A]
case class FlatMap[A, B](sub: IO[A], k: A => IO[B]) extends IO[B]
*/


object IOSimple extends Monad[IOSimple] {

  /*
  @annotation.tailrec def run[A](io: IO[A]): A = io match {
    case Return(a)       => a
    case Suspend(r)      => r()
    case FlatMap(x, f)   => x match {
      case Return(a)     => run(f(a))
      case Suspend(r)    => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
  */

  /*
  def PrintLine(msg: String): IO = new IO {
    def run() = println(msg)
  }

  def empty: IO = new IO { def run() = () }
  */

  override def unit[A](a: => A): IOSimple[A] = new IOSimple[A] { def run() = a }

  override def flatMap[A, B](ma: IOSimple[A])(f: A => IOSimple[B]): IOSimple[B] = ma flatMap f

  def apply[A](a: => A): IOSimple[A] = unit(a)

  def ReadLine: IOSimple[String] = IOSimple { scala.io.StdIn.readLine }

  def PrintLine(msg: String): IOSimple[Unit] = IOSimple { println(msg) }

  def fahrenheitToCelsius(f: Double) = (f - 32) * 5.0 / 9.0

  def converter: IOSimple[Unit] = for {
    _ <- PrintLine("Type degree in Fahrenheit:")
    f <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(f).toString)
  } yield ()

  /*
  def printLine(msg: String): IO[Unit] = Suspend(() => println(msg))

  def suspend[A](a: => IO[A]): IO[A] = Suspend(() => ()) flatMap { _ => a }

  def f1: Int => IO[Int] = x => Return(x)
  def g1 = List.fill(100000)(f1).foldLeft(f1) {
    (a, b) => x => suspend { a(x).flatMap(b) }
  }
  */
}