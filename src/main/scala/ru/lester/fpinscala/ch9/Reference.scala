package ru.lester.fpinscala.ch9

import ReferenceTypes._

import scala.util.matching.Regex

object ReferenceTypes {
  sealed trait Result[+A] {
    def mapError(f: ParseError => ParseError) = this match {
      case Failure(e, c) => Failure(f(e), c)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a, m) => Success(a, n + m)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

  type Parser[+A] = Location => Result[A]

}

object Reference extends Parsers[Parser] {
  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = {
    p(Location(input)) match {
      case Success(a, _) => Right(a)
      case Failure(e, _) => Left(e)
    }
  }


  override def slice[A](p: Parser[A]): Parser[String] = (loc: Location) => p(loc) match {
    case Success(_, n) => Success(loc.input.substring(loc.offset, loc.offset + n), n)
    case f@Failure(_,_) => f
  }
  override def string(s: String): Parser[String] = (loc: Location) =>
    if(loc.input.startsWith(s))
      Success(s, s.length)
    else
      Failure(loc.toError(s).label(s"Expected: $s"), true)

  override def regex(r: Regex): Parser[String] = {
    val msg = s"Input doesn't match expected regex pattern: $r"
    (loc: Location) => {
      r.findPrefixOf(loc.input) match {
        case Some(s) => Success(s, s.length)
        case None => Failure(loc.toError(msg), false)
      }
    }
  }

  override def succeed[A](a: A): Parser[A] = loc => Success(a, 0)

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.push(msg, loc))
  override def label[A](msg: String)(p: Parser[A]): Parser[A] = loc => p(loc).mapError(_.label(msg))

  override def attempt[A](p: Parser[A]): Parser[A] = loc => p(loc).uncommit
  override def or[A](x: Parser[A], y: => Parser[A]): Parser[A] = loc => x(loc) match {
    case Failure(e, false) => y(loc)
    case r => r
  }

  override def flatMap[A, B](f: Parser[A])(g: A => Parser[B]): Parser[B] =
    loc => f(loc) match {
      case Success(a, n) => g(a)(loc.advanceBy(n))
                              .addCommit(n != 0)
                              .advanceSuccess(n)
      case e@Failure(_, _) => e
    }
}
