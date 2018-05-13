object foldable {
  import cats._

  implicit class ListFoldableOps[A: Monoid](list: List[A]) {
    def mapF[B](f: A => B): List[B] =
      list.foldRight(List.empty[B])((e, acc) => f(e) :: acc)
    
    def flatMapF[B](f: A => List[B]): List[B] =
      list.foldRight(List.empty[B])((e, acc) => f(e) ++ acc)
    
    def filterF(f: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((e, acc) => if (f(e)) e :: acc else acc)

    def sumF: A =
      list.foldRight(Monoid[A].empty)(Monoid[A].combine)
  }
}