package ru.lester.fpinscala.ch12

import scala.collection.mutable

sealed trait ST[S, A] { self =>
  protected def run(s: S): (A, S)
  def map[B](f: A => B): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) =  self.run(s)
      (f(a), s1)
    }
  }

  def flatMap[B](f: A => ST[S, B]): ST[S, B] = new ST[S, B] {
    def run(s: S): (B, S) = {
      val (a, s1) = self.run(s)
      f(a).run(s1)
    }
  }
}

object ST {
  def apply[S, A](a: => A): ST[S, A] = {
    lazy val memo = a
    new ST[S, A] {
      def run(s: S): (A, S) = (memo, s)
    }
  }

  val p = new RunnableST[(Int, Int)] {
    def apply[S]: ST[S, (Int, Int)] = for {
      r1 <- STRef(1)
      r2 <- STRef(2)
      x  <- r1.read
      y  <- r2.read
      _  <- r1.write(y + 1)
      _  <- r2.write(x + 1)
      a  <- r1.read
      b <-  r2.read
    } yield (a, b)
  }

  def runST[A](st: RunnableST[A]): A = st.apply[Unit].run(())._1
}

trait STRef[S, A] {
  protected var cell: A

  def read: ST[S, A] = ST(cell)
  def write(a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      cell = a
      ((), s)
    }
  }
}

object STRef {
  def apply[S, A](a: => A): ST[S, STRef[S, A]] = ST(new STRef[S, A] {
    var cell = a
  })
}

trait RunnableST[A] {
  def apply[S]: ST[S, A]
}

sealed trait STMap[S, K, A] {
  protected def value: scala.collection.mutable.HashMap[K, A]
  def size: ST[S, Int] = ST(value.size)
  def +=(k: K, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value += (k -> a)
      ((), s)
    }
  }

  def apply(k: K): ST[S, A] = ST(value(k))
  def get(k: K): ST[S, Option[A]] = ST(value.get(k))

  def -=(k: K): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value -= k
      ((), s)
    }
  }
}

object STMap {
  def empty[S, K, A]: ST[S, STMap[S, K, A]] = ST(new STMap[S, K, A] {
    lazy val value: mutable.HashMap[K, A] = mutable.HashMap.empty[K, A]
  })

  def fromMap[S, K, A](m: Map[K, A]): ST[S, STMap[S, K, A]] = ST(new STMap[S, K, A] {
    lazy val value: mutable.HashMap[K, A] = (mutable.HashMap.newBuilder ++= m).result
  })
}

sealed abstract class STArray[S, A](implicit manifest: Manifest[A]) {
  protected def value: Array[A]
  def size: ST[S, Int] = ST(value.length)

  def write(i: Int, a: A): ST[S, Unit] = new ST[S, Unit] {
    def run(s: S): (Unit, S) = {
      value(i) = a
      ((), s)
    }
  }

  def read(i: Int): ST[S, A] = ST(value(i))

  def freeze: ST[S, List[A]] = ST(value.toList)

  def fill(xs: Map[Int, A]): ST[S, Unit] = xs.foldRight(ST[S, Unit](())){
    case ((i, a), st) => st.flatMap(_ => write(i, a))
  }

  def swap(i: Int, j: Int): ST[S, Unit] = for {
    x <- read(i)
    y <- read(j)
    _ <- write(j, x)
    _ <- write(i, y)
  } yield ()
}

object STArray {
  def apply[S, A: Manifest](sz: Int, el: A): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val value = Array.fill(sz)(el)
  })

  def fromList[S, A: Manifest](xs: List[A]): ST[S, STArray[S, A]] = ST(new STArray[S, A] {
    lazy val value: Array[A] = xs.toArray
  })
}

object Mutable {
  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else {
    val arr = xs.toArray
    def swap(x: Int, y: Int) = {
      val tmp = arr(x)
      arr(x) = arr(y)
      arr(y) = tmp
    }
    def partition(l: Int, r: Int, pivot: Int) = {
      val pivotVal = arr(pivot)
      swap(pivot, r)
      var j = l
      for (i <- l until r) if (arr(i) < pivotVal) {
        swap(i, j)
        j += 1
      }
      swap(j, r)
      j
    }
    def qs(l: Int, r: Int): Unit = if (l < r) {
      val pi = partition(l, r, l + (r - l) / 2)
      qs(l, pi - 1)
      qs(pi + 1, r)
    }
    qs(0, arr.length - 1)
    arr.toList
  }
}

object Immutable {
  def noop[S] = ST[S, Unit](())

  /*
  def movePivot[S](arr: STArray[S, Int], pv: Int, l: Int, r: Int, jR: STRef[S, Int]): ST[S, Unit] = (l until r).foldLeft(noop[S])(
    (st, i) => for {
      _ <- st // Required in order to operate in context of state passed in foldLeft
      v <- arr.read(i)
      _ <-  if(v < pv) (for {
        j <- jR.read
        _ <- arr.swap(i, j)
        _ <- jR.write(j + 1)
      } yield ()) else st
    } yield ())

  def partition[S](arr: STArray[S, Int], left: Int, right: Int, pivot: Int): ST[S, Int] = for {
    pv <- arr.read(pivot)
    _ <- arr.swap(pivot, right)
    jRef <- STRef(left)
    _ <- movePivot(arr, pv, left, right, jRef)
    j <- jRef.read
    _ <- arr.swap(j, right)
  } yield j
  */

  def runTest(): Int = ST.runST(new RunnableST[Int] {
    override def apply[S]: ST[S, Int] = for {
      _ <- noop[S]
      j <- STRef(0)
      /*
      _ <- (0 to 5).foldLeft(noop[S])((s, i) => s flatMap { _ =>
        noop[S].flatMap(_ =>
          (for {
            vj <- j.read
            _  <- j.write(vj + 1)
          } yield ())
        )})
      */
      _ <- (0 to 5).foldLeft(noop[S])((s, i) => for {
          _ <- s
          _ <- noop[S]
          _ <- (for {
              vj <- j.read
              _  <- j.write(vj + 1)
            } yield ())
      } yield ())
      x <- j.read
    } yield x
  })

  def partition[S](a: STArray[S,Int], l: Int, r: Int, pivot: Int): ST[S,Int] = for {
    vp <- a.read(pivot)
    _ <- a.swap(pivot, r)
    j <- STRef(l)
    _ <- (l until r).foldLeft(noop[S])((s, i) => s flatMap { _ =>
      a.read(i).flatMap(vi =>
        if(vi < vp)
          (for {
            vj <- j.read
            _  <- a.swap(i, vj)
            _  <- j.write(vj + 1)
          } yield ())
        else noop[S])})

      /*
      for {
      _ <- s
      vi <- a.read(i)
      _  <- if (vi < vp) (for {
        vj <- j.read
        _  <- a.swap(i, vj)
        _  <- j.write(vj + 1)
      } yield ()) else noop[S]
    } yield ())
    */
    x <- j.read
    _ <- a.swap(x, r)
  } yield x

  def qs[S](arr: STArray[S, Int], left: Int, right: Int): ST[S, Unit] = if(left < right ) (for {
    pi <- partition(arr, left, right, left + (right - left) / 2)
    _  <- qs(arr, left, pi - 1)
    _  <- qs(arr, pi + 1, right)
  } yield ()) else noop[S]

  def quicksort(xs: List[Int]): List[Int] = if (xs.isEmpty) xs else ST.runST(new RunnableST[List[Int]] {
    override def apply[S]: ST[S, List[Int]] = for {
      arr <- STArray.fromList(xs)
      s <- arr.size
      _ <- qs(arr, 0, s - 1)
      sorted <- arr.freeze
    } yield sorted
  })
}