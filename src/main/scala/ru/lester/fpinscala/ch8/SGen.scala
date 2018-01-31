package ru.lester.fpinscala.ch8

/**
 * User: lester
 * Date: 29.01.2015
 * Time: 21:19
 */
case class SGen[+A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def flatMapMy[B](f: A => SGen[B]): SGen[B] = SGen { n =>
    forSize(n).flatMap(a => f(a).forSize(n))
  }

  def map[B](f: A => B): SGen[B] = SGen(forSize andThen (_ map f))

  def flatMap[B](f: A => Gen[B]): SGen[B] = SGen(forSize andThen (_ flatMap f))

  def **[B](sb: SGen[B]): SGen[(A, B)] = SGen(n => this.apply(n) ** sb(n))
}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => Gen.listOfN(n, g)
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen {
    n => Gen.listOfN(n max 1, g)
  }
}
