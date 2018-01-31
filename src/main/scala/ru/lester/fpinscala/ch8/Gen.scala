package ru.lester.fpinscala.ch8

import java.util.concurrent.{Executors, ExecutorService}

import ru.lester.fpinscala.ch6.{State, RNG}
import ru.lester.fpinscala.ch7.Par
import RNG.nonNegativeBetweenS
import Prop._
import SGen._
import ru.lester.fpinscala.ch7.Par
import Par._

/**
 * User: lester
 * Date: 21.01.15
 * Time: 20:41
 */

case class Gen[+A](sample: State[RNG, A]) {
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] = Gen(sample.map2(gb.sample)(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(a => f(a).sample))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(i => Gen.listOfN(i, this))
  //def **[B](gb: Gen[B]): Gen[(A, B)] = this.flatMap(a => gb.map(b => (a, b)))
  def **[B](gb: Gen[B]): Gen[(A, B)] = (this map2 gb)((_, _))
  def unsized: SGen[A] = SGen(_ => this)
}

object ** {
  def unapply[A, B](p: (A, B)) = Some(p)
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = Gen(nonNegativeBetweenS(start, stopExclusive))
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def boolean: Gen[Boolean] = Gen(nonNegativeBetweenS(0, 2).map(i => if(i == 0) false else true))
  def listOfN[A](n: Int, gen: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(gen.sample)))
  def stringN(n: Int) = listOfN(n, Gen.choose(0, 127)).map(_.map(_.toChar).mkString)
  def string = SGen(stringN)
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = boolean.flatMap(b => if (b) g1 else g2)
  def weightedMy[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = choose(0, 101).flatMap(i => {
    val k = if(g1._2 < g2._2) g1._2 / g2._2 else 1 - (g2._2 / g1._2)
    if((i/100) <= k) g2._1 else g1._1
  })

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    Gen(State(RNG.double).flatMap(d => if(d < g1Threshold) g1._1.sample else g2._1.sample))
  }

  val smallInt = Gen.choose(-10, 10)
  val maxProp = forAll(listOf1(smallInt)) { ns =>
    val max = ns.max
    !ns.exists(_ > max)
  }

  val sortedProp = forAll(listOf(smallInt)) { l =>
    val ls = l.sorted
    ls.isEmpty || ls.tail.isEmpty || !ls.zip(ls.tail).exists {case (a, b) => a > b} && !l.exists(!ls.contains(_) && !ls.exists(!l.contains(_)))
  }
}
