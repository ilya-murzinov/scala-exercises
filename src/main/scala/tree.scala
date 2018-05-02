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

trait TreeMonad0 extends Monad[Tree] {
  def pure[A](a: A) = Leaf(a)

  def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] =
    tree match {
      case Leaf(v) => f(v) match {
        case Leaf(fv) => Leaf(fv)
        case Branch(fl, fr) => Branch(fl, fr)
      }

      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }
}

trait StackSafeTreeMonad extends StackUnsafeTreeMonad {
  implicit val treeMonad: Monad[Tree] = new TreeMonad0 {
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      @tailrec
      def loop(open: List[Tree[Either[A, B]]], closed: List[Tree[B]]): List[Tree[B]] = {
        println(s"open: $open")
        println(s"closed: $closed")
        println()
        open match {
          case head :: tail =>
            head match {
              case Leaf(Left(v)) => loop(f(v) :: tail, closed)
              case Leaf(Right(v)) => loop(tail,
                if (closed.isEmpty) List(pure(v))
                else Branch(closed.head, pure(v)) :: closed.tail)
              case Branch(l, r) =>
                l match {
                  case Branch(_, _) => loop(l :: r :: tail, closed)
                  case Leaf(Left(v)) => loop(f(v) :: r :: tail, closed)
                  case Leaf(Right(v)) => loop(r :: tail, pure(v) :: closed)
                }
            }
          case Nil => closed
        }
      }

      loop(List(f(a)), Nil).head
    }
  }
}

trait StackUnsafeTreeMonad {
  implicit val stackUnsafeTreeMonad: Monad[Tree] = new TreeMonad0 {
    def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] =
      flatMap(func(a)) {
        case Left(value) => tailRecM(value)(func)
        case Right(value) => Leaf(value)
      }
  }
}