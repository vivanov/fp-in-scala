package ru.lester.fpinscala.ch11

import ru.lester.fpinscala.ch10.Reader
import ru.lester.fpinscala.ch11.Console.{StateBuffers, ReaderS}
import ru.lester.fpinscala.ch11.Free._
import ru.lester.fpinscala.ch6.State
import State._
import ru.lester.fpinscala.ch7.nonblocking.Par
import ru.lester.fpinscala.ch7.nonblocking.Par.Par

import ru.lester.fpinscala.ch10.Monad
import Monad._

import ru.lester.fpinscala.ch10.Reader
import Reader._

case class Buffers(in: List[String], out: List[String])

sealed trait Console[A] {
  def toPar: Par[A]
  def toThunk: () => A
  def toReaderS: ReaderS[A]
  def toStateBuffers: StateBuffers[A]
}

case object ReadLine extends Console[Option[String]] {

  override def toPar: Par[Option[String]] = Par.lazyUnit(run)

  override def toThunk: () => Option[String] = () => run

  override def toReaderS: ReaderS[Option[String]] = Reader { _ => Some("test") }

  override def toStateBuffers: StateBuffers[Option[String]] = for {
    buf <- get[Buffers]
    _ <- set(Buffers(buf.in.tail, buf.out))
  } yield buf.in.headOption

  def run: Option[String] =
    try { Some(scala.io.StdIn.readLine()) }
    catch { case e: Exception => None }
}

case class PrintLine(line: String) extends Console[Unit] {
  override def toPar: Par[Unit] = Par.lazyUnit(println(line))

  override def toThunk: () => Unit = () => println(line)

  override def toReaderS: ReaderS[Unit] = Reader { _ => () }

  override def toStateBuffers: StateBuffers[Unit] = for {
    buf <- get[Buffers]
    _ <- set(Buffers(buf.in, line :: buf.out))
  } yield ()
}

object Console {
  type ConsoleIO[A] = Free[Console, A]
  type ReaderS[A] = Reader[String, A]
  type StateBuffers[A] = State[Buffers, A]

  def readLine(): ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLine(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))

  val f1: ConsoleIO[Option[String]] = for {
    _  <- printLine("I can only interact with Console...")
    ln <- readLine()
  } yield ln

  val consoleToFunction0 = new (Console ~> Function0) {
    def apply[A](c: Console[A]): () => A = c.toThunk
  }

  val consoleToPar = new (Console ~> Par) {
    def apply[A](c: Console[A]): Par[A] = c.toPar
  }

  val consoleToReaderS = new (Console ~> ReaderS) {
    override def apply[A](fa: Console[A]): ReaderS[A] = fa.toReaderS
  }

  val consoleToStateBuffers = new (Console ~> StateBuffers) {
    override def apply[A](fa: Console[A]): StateBuffers[A] = fa.toStateBuffers
  }

  // Isn't stack-safe as flatMap for Function0 is not stack-safe
  def runConsoleFunction0[A](free: Free[Console, A]): () => A = runFree[Console, Function0, A](free)(consoleToFunction0)
  def runConsolePar[A](free: Free[Console, A]): Par[A] = runFree[Console, Par, A](free)(consoleToPar)
  // These two below aren't stack-safe either. Need to change representation to ReaderS[TailRec[A]] and StateBuffers[TailRec[A]] accordingly
  def runConsoleReaderS[A](free: Free[Console, A]): ReaderS[A] = runFree[Console, ReaderS, A](free)(consoleToReaderS)
  def runConsoleStateBuffers[A](free: Free[Console, A]): StateBuffers[A] = runFree[Console, StateBuffers, A](free)(consoleToStateBuffers)

  def runConsole[A](free: Free[Console, A]): A = runTailRec(translate[Console, Function0, A](free)(consoleToFunction0))
}

case class ConsoleReader[A](run: String => A) {

}