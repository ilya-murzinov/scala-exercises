import cats._
import cats.implicits._

object free {
  case class Fix[F[_]](unfix: F[Fix[F]])
  case class Cofree[F[_], A](head: A, tail: F[Cofree[F, A]])
  case class Free[F[_], A](resume: Either[A, F[Free[F, A]]])

  object Cofree {
    implicit def cofreeComonad[F[_]](
        implicit FF: Functor[F]): Comonad[Cofree[F, ?]] =
      new Comonad[Cofree[F, ?]] {
        override def extract[A](x: Cofree[F, A]): A = x.head

        override def coflatMap[A, B](fa: Cofree[F, A])(
            f: Cofree[F, A] => B): Cofree[F, B] =
          Cofree(f(fa), FF.map(fa.tail)(a => coflatMap(a)(f)))

        override def map[A, B](fa: Cofree[F, A])(f: A => B): Cofree[F, B] =
          Cofree(f(fa.head), FF.map(fa.tail)(a => map(a)(f)))
      }

    implicit def cofreeFlatMap[F[_]](
        implicit FF: Functor[F]): FlatMap[Cofree[F, ?]] =
      new FlatMap[Cofree[F, ?]] {
        override def flatMap[A, B](fa: Cofree[F, A])(
            f: A => Cofree[F, B]): Cofree[F, B] =
          flatMap(f(fa.head))(b =>
            Cofree(b, FF.map(fa.tail)(t => flatMap(t)(f))))

        override def tailRecM[A, B](a: A)(
            f: A => Cofree[F, Either[A, B]]): Cofree[F, B] = ???

        override def map[A, B](fa: Cofree[F, A])(f: A => B): Cofree[F, B] =
          Cofree(f(fa.head), FF.map(fa.tail)(a => map(a)(f)))
      }
  }

  object Free {
    implicit def freeMonad[F[_]](implicit FF: Functor[F]): Monad[Free[F, ?]] =
      new Monad[Free[F, ?]] {
        override def flatMap[A, B](fa: Free[F, A])(
            f: A => Free[F, B]): Free[F, B] =
          fa match {
            case Free(Left(a)) => f(a)
            case Free(Right(ff)) => Free(FF.map(ff)(e => flatMap(e)(f)).asRight)
          }

        override def pure[A](x: A): Free[F, A] = Free(x.asLeft)

        override def tailRecM[A, B](a: A)(
            f: A => Free[F, Either[A, B]]): Free[F, B] = ???
      }
  }
}
