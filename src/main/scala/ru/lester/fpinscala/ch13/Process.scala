package ru.lester.fpinscala.ch13

import java.io.FileWriter

import ru.lester.fpinscala.ch10.Monad
import ru.lester.fpinscala.ch11.{Task, Files, IO, unsafePerformIO}
import ru.lester.fpinscala.ch7.nonblocking.Par
import ru.lester.fpinscala.ch7.nonblocking.Par.Par
import ru.lester.fpinscala.ch7.nonblocking.Par.Par

import scala.annotation.tailrec

trait Process[F[_], O] {
  import Process._

  def map[O2](f: O => O2): Process[F, O2] = this match {
    case Halt(e)    => Halt(e)
    case Emit(h, t) => Try { Emit(f(h), t map f) }
    case Await(req, recv) => Await(req, recv andThen (_ map f))
  }

  def onHalt(f: Throwable => Process[F, O]): Process[F, O] = this match {
    case Halt(e)          => Try(f(e))
    case Emit(h, t)       => Emit(h, t.onHalt(f))
    case Await(req, recv) => Await(req, recv andThen(_.onHalt(f)))
  }

  def ++(p: => Process[F, O]): Process[F, O] = this.onHalt  {
    case End => Try(p)
    case err => Halt(err)
  }

  def flatMap[O2](f: O => Process[F, O2]): Process[F, O2] = this match {
    case Halt(e)          => Halt(e)
    case Emit(h, t)       => Try(f(h)) ++ t.flatMap(f)
    case Await(req, recv) => Await(req, recv andThen (_ flatMap f))
  }

  def runLog(implicit ev: MonadCatch[F]): F[IndexedSeq[O]] = {
    def go(cur: Process[F, O], acc: IndexedSeq[O]): F[IndexedSeq[O]] = cur match {
      case Emit(h, t)       => go(t, acc :+ h)
      case Halt(End)        => ev.unit(acc)
      case Halt(err)        => ev.fail(err)
      case Await(req, recv) => ev.flatMap(ev.attempt(req))(res => go(Try(recv(res)), acc))
    }
    go(this, IndexedSeq())
  }

  def onComplete(p: => Process[F, O]): Process[F, O] = this.onHalt {
    case End => p.asFinalizer
    case err => p.asFinalizer ++ Halt(err)
  }

  def asFinalizer: Process[F, O] = this match {
    case Emit(h, t) => Emit(h, t.asFinalizer)
    case Halt(e)    => Halt(e)
    case Await(req, recv) => await(req) {
      case Left(Kill) => this.asFinalizer
      case x          => recv(x)
    }
  }

  def drain[O2]: Process[F, O2] = this match {
    case Halt(e)    => Halt(e)
    case Emit(_, t) => t.drain
    case Await(req, recv) => Await(req, recv andThen (_.drain))
  }

  def repeat: Process[F, O] = this ++ this.repeat

  @tailrec
  final def kill[O2]: Process[F, O2] = this match {
    case Await(req, recv) => recv(Left(Kill)).drain.onHalt {
      case Kill => Halt(End)
      case e    => Halt(e)
    }
    case Halt(e) => Halt(e)
    case Emit(h, t) => t.kill
  }

  def |>[O2](p2: Process1[O, O2]): Process[F, O2] = p2 match {
    case Halt(e)          => this.kill onHalt { e2 => Halt(e) ++ Halt(e2)}
    case Emit(h, t)       => Emit(h, this |> t)
    case Await(req, recv) => this match {
      case Halt(err) => Halt(err) |> recv(Left(err))
      case Emit(h ,t) => t |> Try(recv(Right(h)))
      case Await(req0, recv0) => await(req0)(recv0 andThen(_ |> p2))
    }
  }

  def pipe[O2](p2: Process1[O, O2]): Process[F, O2] = this |> p2

  def filter(p: O => Boolean): Process[F, O] = this |> Process.filter(p)

  def tee[O2, O3](p2: Process[F, O2])(t: Tee[O, O2, O3]): Process[F, O3] = t match {
    case Halt(e)    => this.kill onComplete p2.kill onComplete Halt(e)
    case Emit(h, t) => Emit(h, (this tee p2)(t))
    case Await(side, recv) => side.get match {
      case Left(isO)   => this match {
        case Halt(e)            => p2.kill onComplete Halt(e)
        case Emit(o, ot)        => (ot tee p2) (Try(recv(Right(o))))
        case Await(reqL, recvL) => await(reqL)(recvL andThen (p => p.tee(p2)(t)))
      }
      case Right(isO2) => p2 match {
        case Halt(e)      => this.kill onComplete Halt(e)
        case Emit(o2, ot) => (this tee ot) (Try(recv(Right(o2))))
        case Await(reqR, recvR) => await(reqR)(recvR andThen (p => this.tee(p)(t)))
      }
    }
  }

  def zipWith[O2, O3](p2: Process[F, O2])(f: (O, O2) => O3): Process[F, O3] =
    (this tee p2) (Process.zipWith(f))

  def zip[O2](p2: Process[F, O2]): Process[F, (O, O2)] = zipWith(p2) ((_, _))

  def to(sink: Sink[F, O]): Process[F, Unit] = join { (this zipWith sink) ((o, f) => f(o))}

  def through[O2](p2: Channel[F, O, O2]): Process[F, O2] = join { (this zipWith p2) ((o, f) => f(o)) }
}

object Process {
  case class Await[F[_], A, O](req: F[A], recv: Either[Throwable, A] => Process[F, O]) extends Process[F, O]
  case class Emit[F[_], O](head: O, tail: Process[F, O]) extends Process[F, O]
  case class Halt[F[_], O](end: Throwable) extends Process[F, O]

  case object End extends Exception
  case object Kill extends Exception

  def Try[F[_], O](p: => Process[F, O]): Process[F, O] = {
    try p
    catch { case e: Throwable => Halt(e) }
  }

  def await[F[_], A, O](req: F[A]) (recv: Either[Throwable, A] => Process[F, O]): Process[F, O] = Await(req, recv)
  def emit[F[_], O](head: O, tail: Process[F, O]): Process[F, O] = Emit(head, tail)

  def runLog[O](src: Process[IO, O]): IO[IndexedSeq[O]] = IO {
    val E = java.util.concurrent.Executors.newFixedThreadPool(4)
    @tailrec
    def go(cur: Process[IO, O], acc: IndexedSeq[O]): IndexedSeq[O] = cur match {
      case Emit(h, t)       => go(t, acc :+ h)
      case Halt(End)        => acc
      case Halt(err)        => throw err
      case Await(req, recv) =>
        val next = try { recv(Right(unsafePerformIO(req)(E))) } catch { case err:Throwable => recv(Left(err)) }
        go(next, acc)
    }
    try { go(src, IndexedSeq()) }
    finally E.shutdown()
  }

  import java.io.{BufferedReader, FileReader}

  val p: Process[IO, String] = await( IO(new BufferedReader(new FileReader("lines.txt"))) ) {
    case Right(b) => {
      lazy val next: Process[IO, String] = await( IO(b.readLine) ) {
        case Left(e)     => await( IO(b.close) ) (_ => Halt(e))
        case Right(line) => if (line eq null) Halt(End) else Emit(line, next)
      }
      next
    }
    case Left(e) => Halt(e)
  }

  val indSeq: IO[IndexedSeq[String]] = runLog(p)

  def resource[R, O](aquire: IO[R])(use: R => Process[IO, O])(release: R => Process[IO, O]): Process[IO, O] =
    eval(aquire).flatMap(r => use(r).onComplete(release(r)))

  def resource_[R, O](aquire: IO[R])(use: R => Process[IO, O])(release: R => IO[Unit]): Process[IO, O] =
    resource(aquire)(use)(release andThen(eval_[IO, Unit, O]))

  def eval[F[_], A](fa: F[A]): Process[F, A] = await[F, A, A](fa) {
    case Left(err)   => Halt(err)
    case Right(a)    => Emit(a, Halt(End))
  }

  def eval_[F[_], A, B](fa: F[A]): Process[F, B] = eval(fa).drain[B]

  def lines(fileName: String): Process[IO, String] = resource
    { IO(io.Source.fromFile(fileName)) }
    { src =>
      lazy val iter = src.getLines()
      def step = if (iter.hasNext) Some(iter.next) else None
      lazy val lines: Process[IO, String] = eval { IO(step) } flatMap {
        case None       => Halt(End)
        case Some(line) => Emit(line, lines)
      }
      lines
    }
    { src => eval_ { IO(src.close()) } }

  case class Is[I]() {
    sealed trait f[X]
    val Get = new f[I] {}
  }
  def Get[I] = Is[I]().Get

  type Process1[I, O] = Process[Is[I]#f, O]

  def await1[I, O](recv: I => Process1[I, O], fallback: Process1[I, O] = halt1[I, O]): Process1[I, O] =
    Await(Get[I], (e: Either[Throwable, I]) => e match {
      case Left(End) => fallback
      case Left(err) => Halt(err)
      case Right(i)  => Try(recv(i))
    })

  def emit1[I, O](h: O, t1: Process1[I, O] = halt1[I, O]): Process1[I, O] = emit(h, t1)

  def halt1[I, O]: Process1[I, O] = Halt[Is[I]#f, O](End)

  def lift[I, O](f: I => O): Process1[I, O] = await1[I, O](i => emit1(f(i))) repeat
  def filter[I](f: I => Boolean): Process1[I, I] = await1[I, I](i => if(f(i)) emit1(i) else halt1) repeat

  case class T[I, I2]() {
    sealed trait f[X] { def get: Either[I => X, I2 => X] }
    val L = new f[I]  { def get = Left(identity) }
    val R = new f[I2] { def get = Right(identity)}
  }

  def L[I, I2] = T[I, I2]().L
  def R[I, I2] = T[I, I2]().R

  type Tee[I, I2, O] = Process[T[I, I2]#f, O]

  def haltT[I, I2, O]: Tee[I, I2, O] = Halt[T[I, I2]#f, O](End)

  def awaitL[I, I2, O](recv: I => Tee[I, I2, O], fallback: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = await[T[I, I2]#f, I, O](L) {
    case Left(End) => fallback
    case Left(err) => Halt(err)
    case Right(i)  => Try(recv(i))
    }
  def awaitR[I, I2, O](recv: I2 => Tee[I, I2, O], fallback: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = await[T[I, I2]#f, I2, O](R) {
    case Left(End) => fallback
    case Left(err) => Halt(err)
    case Right(i2) => Try(recv(i2))
  }

  def emitT[I, I2, O](h: O, t1: Tee[I, I2, O] = haltT[I, I2, O]): Tee[I, I2, O] = emit(h, t1)

  def zipWith[I, I2, O](f: (I, I2) => O): Tee[I, I2, O] =
    awaitL[I, I2, O](i => awaitR(i2 => emitT(f(i, i2)))) repeat

  def zip[I, I2]: Tee[I, I2, (I, I2)] = zipWith((_, _))

  type Sink[F[_], O] = Process[F, O => Process[F, Unit]]

  def fileW(name: String, append: Boolean = false): Sink[IO, String] =
    resource[FileWriter, String => Process[IO, Unit]]
      { IO(new FileWriter(name, append)) }
      { w => constant { (s: String) => eval[IO, Unit] { IO(w.write(s)) } } }
      { w => eval_ { IO(w.close()) }}

  def constant[A](a: A): Process[IO, A] = eval[IO, A](IO(a)).flatMap { a => Emit(a, constant(a)) }

  def join[F[_], O](p: Process[F, Process[F, O]]): Process[F, O] = p.flatMap(identity)

  def id[I]: Process1[I, I] = await1((i: I) => emit(i, id))

  def window2[I]: Process1[I, (Option[I], I)] = {
    def go(prev: Option[I]): Process1[I, (Option[I], I)] = await1[I, (Option[I], I)](i => emit1(prev -> i) ++ go(Some(i)))
    go(None)
  }

  def intersperse[I](sep: I): Process1[I, I] = await1(i => emit1(i) ++ id.flatMap(i => emit1(sep) ++ emit1(i)))

  //val url = this.getClass().getResource("/farenheit.txt")
  /*
  val res = Process.runLog(converter)
  val pool = Executors.newFixedThreadPool(4)
  unsafePerformIO(res)(pool)
  */

  val converter: Process[IO, Unit] = lines("/home/vvivanov/projects/test/fp-in-scala/src/main/resources/farenheit.txt")
                                      .filter(!_.startsWith("#"))
                                      .map(s => Files.fahrenheitToCelsius(s.toDouble).toString)
                                      .pipe(intersperse("\n"))
                                      .to(fileW("/home/vvivanov/projects/test/fp-in-scala/src/main/resources/celsius.txt"))
                                      .drain

  type Channel[F[_], I, O] = Process[F, I => Process[F, O]]

  type Writer[F[_], W, O] = Process[F, Either[W, O]]

  import java.sql.{Connection, PreparedStatement, ResultSet}

  def query(conn: IO[Connection]): Channel[IO, Connection => PreparedStatement, Map[String, Any]] =
    resource_
      { conn }
      { c => constant { (q: Connection => PreparedStatement) =>
          resource_ {
            IO {
              val rs = q(c).executeQuery()
              val ncols = rs.getMetaData.getColumnCount
              val cols = (1 to ncols).map(rs.getMetaData.getColumnName)
              (rs, cols)
            }
          }
          { case (rs, cols) =>
              def step = if(!rs.next) None else Some(cols.map(c => (c, rs.getObject(c): Any)).toMap)
              lazy val rows: Process[IO, Map[String, Any]] = eval(IO(step)).flatMap {
                case None      => Halt(End)
                case Some(row) => Emit(row, rows)
              }
              rows
          }
          {
            p => IO(p._1.close) // Close the ResultSet
          }
      }}
      { c => IO(c.close) }
}

trait MonadCatch[F[_]] extends Monad[F] {
  def attempt[A](fa: F[A]): F[Either[Throwable, A]]
  def fail[A](err: Throwable): F[A]
}

object MonadCatch {
  implicit def taskMonadCatch = new MonadCatch[Task] {

    override def attempt[A](fa: Task[A]): Task[Either[Throwable, A]] = fa.attempt

    override def fail[A](err: Throwable): Task[A] = Task.fail(err)

    override def unit[A](a: => A): Task[A] = Task.unit(a)

    override def flatMap[A, B](ta: Task[A])(f: A => Task[B]): Task[B] = ta flatMap f
  }
}