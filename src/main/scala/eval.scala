import cats._

object fold {
  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
    foldRightEval(as, Eval.now(acc))(((a, b) => b.map(fn(a, _)))).value

  def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
    as match {
      case head :: tail => 
        Eval.defer(fn(head, foldRight(tail, acc)(fn)))
      case Nil => acc
    }
}
