import cats._
import scala.annotation.tailrec

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {
  implicit def eqTree[A: Eq]: Eq[Tree[A]] = Eq.fromUniversalEquals

  implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
    def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
      tree match {
        case Leaf(v) => Leaf(f(v))
        case Branch(t1, t2) => Branch(map(t1)(f), map(t2)(f))
      }
  }
}

trait StackSafeTreeMonad extends StackUnsafeTreeMonad {
  implicit val treeMonad: Monad[Tree] = new Monad[Tree] {
    override def pure[A](a: A): Tree[A] = stackUnsafeTreeMonad.pure(a)

    override def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] =
      stackUnsafeTreeMonad.flatMap(tree)(f)
    
    //@tailrec
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = ???
  }
}

trait StackUnsafeTreeMonad {
  implicit val stackUnsafeTreeMonad: Monad[Tree] = new Monad[Tree] {
    def pure[A](a: A) = Leaf(a)

    def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] =
      tree match {
        case Leaf(v) => f(v) match {
          case Leaf(fv) => Leaf(fv)
          case Branch(fl, fr) => Branch(fl, fr)
        }

        case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
      }

    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      f(a) match {
        case Leaf(Left(v)) => tailRecM(v)(f)
        case Leaf(Right(v)) => Leaf(v)
        case Branch(l, r) =>
          val fl = flatMap(l) {
            case Left(fll) => tailRecM(fll)(f)
            case Right(flr) => Leaf(flr)
          }
          val fr = flatMap(r) {
            case Left(frl) => tailRecM(frl)(f)
            case Right(frr) => Leaf(frr)
          }
          Branch(fl, fr)
      }
  }
}