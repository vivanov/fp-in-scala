package ru.lester.fpinscala.ch8

import java.util.concurrent.{Executors, ExecutorService}

import ru.lester.fpinscala.ch5.Stream
import ru.lester.fpinscala.ch6.RNG
import ru.lester.fpinscala.ch7.Par
import Gen._
import Par._

import Prop._

/**
 * User: lester
 * Date: 21.01.15
 * Time: 19:57
 */

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successCount: SuccessCount) extends Result {
  def isFalsified = true
}

case object Proved extends Result {
  override def isFalsified = false
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def my_&&(otherProp: Prop): Prop = Prop {
    (max, n , rng) => {
      val res1 = this.run(max, n, rng)
      val res2 = otherProp.run(max, n, rng)
      if(res1.isFalsified) res1
      else if(res2.isFalsified) res2
      else Passed
    }
  }

  def my_||(otherProp: Prop): Prop = Prop {
    (max, n , rng) => {
      val res1 = this.run(max, n, rng)
      val res2 = otherProp.run(max, n, rng)
      if(!res1.isFalsified) res1
      else if(!res2.isFalsified) res2
      else res1
    }
  }

  def &&(otherProp: Prop): Prop = Prop {
    (max, n , rng) => run(max, n, rng) match {
        case Passed | Proved => otherProp.run(max, n, rng)
        case x => x
    }
  }

  def ||(otherProp: Prop): Prop = Prop {
    (max, n , rng) => run(max, n, rng) match {
      case Falsified(msg, _) => otherProp.tag(msg).run(max, n, rng)
    }
  }

  def tag(otherMsg: String): Prop = Prop {
    (max, n, rng) => run(max, n, rng) match {
      case Falsified(msg, cnt) => Falsified(otherMsg + "\n" + msg, cnt)
      case x => x
    }
  }

  //def check: Result
}

object Prop {
  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zipUF(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if(f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))
  def buildMsg[A](a: A, e: Exception): String =
    s"test case: $a\n" +
    s"generated and exception: ${e.getMessage}\n" +
    s"stack trace: ${e.getStackTrace.mkString("\n")}"

  def apply(f: (TestCases, RNG) => Result): Prop = Prop((_, n, rng) => f(n, rng))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] = Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop {(max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
      prop.run(max, n ,rng)
  }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if(p) Proved else Falsified("()", 0)
  }

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = RNG.Simple(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) => println(s"! Falsified after $n passed tests: \n $msg")
      case Passed => println(s"+ OK, passed $testCases tests.")
      case Proved => println(s"+ OK, proved property.")
    }

  val ES: ExecutorService = Executors.newCachedThreadPool

  val p2 = check {
    val p = Par.map(Par.unit(1)) (_ + 1)
    val p2 = Par.unit(2)
    p(ES).get == p2(ES).get
  }

  def equal[A, B](pa: Par[A], pb: Par[B]): Par[Boolean] = Par.map2(pa, pb)(_ == _)


  val S = weighted(choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
                   Gen.unit(Executors.newCachedThreadPool) -> .25)

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) {case s ** a => f(a)(s).get}

  def checkPar(p: => Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  val p3 = checkPar {
    equal(Par.map(Par.unit(1)) (_ + 1),
          Par.unit(2)
    )
  }

  val pint = Gen.choose(1, 10) map Par.unit

  val p4 = forAllPar(pint)(n => equal(Par.fork(n), n)) tag "fork"
}
