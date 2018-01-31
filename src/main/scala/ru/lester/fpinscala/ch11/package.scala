package ru.lester.fpinscala

import ru.lester.fpinscala.ch10.Monad
import ru.lester.fpinscala.ch11.Free
import ru.lester.fpinscala.ch7.nonblocking.Par
import ru.lester.fpinscala.ch7.nonblocking.Par._
import ru.lester.fpinscala.ch7.nonblocking._

package object ch11 {
  import java.util.concurrent.ExecutorService

  type IO[A] = Free[Par, A]
  def IO[A](a: => A): IO[A] = Suspend[Par, A] { Par.delay(a) }

  def par[A](pa: Par[A]): IO[A] = Suspend[Par, A] { pa }

  def unsafePerformIO[A](io: IO[A])(implicit es: ExecutorService): A =
    Par.run(es) { Free.run(io)(Monad.parMonad) }
}
