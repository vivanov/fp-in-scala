package ru.lester.fpinscala.ch11

import ru.lester.fpinscala.ch7.nonblocking.Par._

trait HandleR
trait HandleW

sealed trait Files[A]
case class OpenRead(file: String) extends Files[HandleR]
case class OpenWrite(file: String) extends Files[HandleW]
case class ReadLineH(handle: HandleR) extends Files[Option[String]]
case class WriteLineH(handle: HandleW, line: String) extends Files[Unit]


object Files {

  type IO[A] = Free[Par, A]

  // It works, but it's not composable like for example simple List combinators
  def loop(hR: HandleR, hW: HandleW): Free[Files, Unit] = for {
    line <- Suspend[Files, Option[String]](ReadLineH(hR))
    cs = line match {
      case None => Return[Files, Unit](())
      case Some(l) => Suspend[Files, Unit](WriteLineH(hW, fahrenheitToCelsius(l.toDouble).toString)).flatMap(_ => loop(hR, hW))
    }
  } yield ()


  def convertFiles(file1: String, file2: String): Free[Files, Unit] = for {
    hR <- Suspend(OpenRead(file1))
    hW <- Suspend(OpenWrite(file2))
    _ <- loop(hR, hW)
  } yield ()

  def fahrenheitToCelsius(f: Double) = (f - 32) * 5.0 / 9.0
}

