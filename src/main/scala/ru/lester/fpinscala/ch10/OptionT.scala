package ru.lester.fpinscala.ch10

case class OptionT[M[_]: Monad, A](value: M[Option[A]]) {
  def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] = {
    OptionT(
      implicitly[Monad[M]].flatMap(value) {
        case None => implicitly[Monad[M]].unit(None)
        case Some(a) => f(a).value
      }
    )
  }
}
