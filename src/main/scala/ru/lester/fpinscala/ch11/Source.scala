package ru.lester.fpinscala.ch11

import java.nio.ByteBuffer
import java.nio.channels.AsynchronousFileChannel

import ru.lester.fpinscala.ch7.nonblocking.Par._

trait Source {
  def readBytes(numBytes: Int, callback: Either[Throwable, Array[Byte]] => Unit): Unit
}

object Source {
  def nonBlockingRead(source: Source, numBytes: Int): Par[Either[Throwable, Array[Byte]]] = async {
    (cb: Either[Throwable, Array[Byte]] => Unit) => source.readBytes(numBytes, cb)
  }

  def readPar(source: Source, numBytes: Int): Free[Par, Either[Throwable, Array[Byte]]] = Suspend[Par, Either[Throwable, Array[Byte]]](nonBlockingRead(source, numBytes))

  val src: Source = ???

  val prog: Free[Par, Unit] = for {
    chunk1 <- readPar(src, 1024)
    chunk2 <- readPar(src, 1024)
  } yield unit(())

  /*
  def read(file: AsynchronousFileChannel, fromPosition: Long, numBytes: Int): Par[Either[Throwable, Array[Byte]]] = async {
    (cb: Either[Throwable, Array[Byte]] => Unit) => file.read(ByteBuffer.allocate(numBytes), fromPosition, null, cb)
  }
  */
}