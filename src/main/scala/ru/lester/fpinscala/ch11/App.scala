package ru.lester.fpinscala.ch11

import ru.lester.fpinscala.ch11.Free._
import ru.lester.fpinscala.ch10.Monad
import Monad._

import ru.lester.fpinscala.ch7.nonblocking.Par
import ru.lester.fpinscala.ch7.nonblocking.Par._

abstract class App {
  import java.util.concurrent._

  type IO[A] = Free[Par, A]

  def unsafePerformIO[A](ioa: IO[A])(pool: ExecutorService): A =
    Par.run(pool)(Free.run(ioa)(parMonad))

  def main(args: Array[String]): Unit = {
    val pool = Executors.newFixedThreadPool(8)
    unsafePerformIO(pureMain(args))(pool)
  }

  // Implement in subclass
  def pureMain(args: IndexedSeq[String]): IO[Unit]
}
