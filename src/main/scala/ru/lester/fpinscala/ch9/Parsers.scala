package ru.lester.fpinscala.ch9

import ru.lester.fpinscala.ch8._
import Prop._

import scala.util.matching.Regex

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val column = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String) = ParseError(List((this, msg)))
  def advanceBy(n: Int): Location = copy(offset = offset + n)
}

case class ParseError(stack: List[(Location, String)]) {
  def push(msg: String, loc: Location): ParseError = copy(stack = (loc, msg) :: stack)
  def label[A](s: String): ParseError = ParseError(latestLoc.map((_, s)).toList)
  def latestLoc: Option[Location] = latest map (_._1)
  def latest: Option[(Location, String)] = stack.lastOption
}


trait Parsers[Parser[+A]] { self =>
  def char(c: Char): Parser[Char] = string(c.toString) map (_.charAt(0))
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def or[A](p1: Parser[A], p2: => Parser[A]): Parser[A]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = if(n == 0) succeed(Nil) else map2(p, listOfN(n - 1, p))(_ :: _)
  def many[A](p: Parser[A]): Parser[List[A]] =  map2(p, many(p))(_ :: _) | succeed(Nil)
  def map[A, B](p: Parser[A])(f: A => B): Parser[B] = p flatMap (f andThen succeed)
  // size will be called on List, therefore it will take O(n)
  //val numA: Parser[Int] = char('a').many.map(_.size)

  // Circular definition with map
  def defaultSucceed[A](a: A): Parser[A] = string("") map (_ => a)
  def succeed[A](a: A): Parser[A]

  def slice[A](p: Parser[A]): Parser[String]
  // size will be called on String, therefore it will take O(1)
  //val numA: Parser[Int] = char('a').many.slice.map(_.size)

  def product[A, B](p1: Parser[A], p2: => Parser[B]): Parser[(A, B)] = p1 flatMap (a => p2  map (b => (a, b)))
  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] = p1 flatMap (a => p2 map (b => f(a, b)))

  def many1[A](p: Parser[A]): Parser[List[A]] = map2(p, many(p))(_ :: _)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  def label[A](msg: String)(p: Parser[A]): Parser[A]
  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

  // Catch error here
  def ctxDepParser = for {
    s <- """\d+""".r
    n = s.toInt
    _ <- listOfN(n, char('a'))
  } yield n

  // Catch error here
  def ctxDepParserF = """\d+""".r flatMap(s => { val n = s.toInt; listOfN(n, char('a').map(_ => n))})

  implicit def string(s: String): Parser[String]

  implicit def regex(r: Regex): Parser[String]


  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def product[B](p2: => Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equalLaw[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equalLaw(p, p.map(a => a))(in)
  }
}


