package ru.lester.fpinscala.ch10

trait Kleisli[F[_]] {
  def compose[A, B, C](f: A => F[B], g: B => F[C])(implicit ev: Monad[F]): A => F[C] = a => implicitly[Monad[F]].flatMap(f(a))(b => g(b))
  // compose should be called such a way that F[A] can be applied to it and get F[B] as the result
  def flatMap[A, B](fa: F[A])(f: A => F[B])(implicit ev: Monad[F]): F[B] = compose(identity[F[A]], f).apply(fa)
}
