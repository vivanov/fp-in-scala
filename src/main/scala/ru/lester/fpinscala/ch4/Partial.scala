package ru.lester.fpinscala.ch4

/**
 * Created with IntelliJ IDEA.
 * User: lester
 * Date: 22.10.14
 * Time: 21:15
 */

// Similar to scalaz.Validation

object Partial {
  sealed trait Partial[+A, +B] {
    def map[C](f: B => C): Partial[A, C] = this match {
      case Errors(ers) => Errors(ers)
      case Success(b) => Success(f(b))
    }

    def map2[AA >: A, C, D](c: Partial[AA, C])(f: (B, C) => D): Partial[AA, D] = this match {
      case Errors(ers) => c match {
        case Errors(oers) => Errors(ers ++ oers)
        case _ => Errors(ers)
      }
      case Success(bb) => c match {
        case Errors(oers) => Errors(oers)
        case Success(cc) => Success(f(bb, cc))
      }
    }
  }
  case class Errors[+A](value: Seq[A]) extends Partial[A, Nothing]
  case class Success[+B](value: B) extends Partial[Nothing, B]

  def sequence[A, B](ps: List[Partial[A, B]]): Partial[A, List[B]] = ps match {
    case Nil => Success(Nil)
    case x :: xs => x.map2(sequence(xs))(_ :: _)
  }

  def traverse[A, B, C](as: List[B])(f: B => Partial[A, C]): Partial[A, List[C]] = as match{
    case Nil => Success(Nil)
    case x :: xs => f(x).map2(traverse(xs)(f))(_ :: _)
  }
}
