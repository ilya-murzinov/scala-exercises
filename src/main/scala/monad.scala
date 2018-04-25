object monad {
  type Id[A] = A

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    
    def flatMap[A, B](value: F[A])(func: A => F[B]): F[B]
    
    def map[A, B](value: F[A])(func: A => B): F[B] = flatMap(value)(func andThen pure)
  }

  object Monad {
    implicit val idMonad: Monad[Id] = new Monad[Id] {
      def pure[A](a: A) = a
      def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
      override def map[A, B](value: Id[A])(func: A => B): Id[B] = flatMap(value)(func)
    }
  }
}
