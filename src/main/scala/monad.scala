import cats.Id
import cats.Monad

import scala.annotation.tailrec

object monad {
  import cats.implicits._

  def product[M[_]: Monad, A, B](mx: M[A], my: M[B]): M[(A, B)] =
    for {
      x <- mx
      y <- my
    } yield (x, y)
}

object id {
  trait MyMonad[F[_]] {
    def pure[A](a: A): F[A]

    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]

    def map[A, B](value: F[A])(func: A => B): F[B] =
      flatMap(value)(func andThen pure)
  }

  implicit val idMyMonad: MyMonad[Id] = new MyMonad[Id] {
    def pure[A](a: A) = a
    def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
    override def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
  }

  implicit val idMonad: Monad[Id] = new Monad[Id] {
    override def pure[A](x: A): Id[A] = x

    override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] =
      f(a) match {
        case Left(v) => tailRecM(v)(f)
        case Right(v) => v
      }
  }
}
