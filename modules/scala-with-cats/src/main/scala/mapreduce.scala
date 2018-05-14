import cats._
import cats.instances.all._
import cats.syntax.all._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object mapreduce {
  implicit class VectorOps[A](values: Vector[A]) {
    val cpuCount = Runtime.getRuntime.availableProcessors

    def foldMap[B: Monoid](f: A => B)(implicit M: Monoid[B]): B =
      values.map(f).foldLeft(M.empty)(M.combine)

    def parallelFoldMap[B](f: A => B)(implicit M: Monoid[B]): Future[B] = {
      val batchSize = (values.size / cpuCount) + 1

      Future
        .sequence(
          values
            .grouped(batchSize)
            .map(batch => Future(batch.foldMap(f)))
        )
        .map(_.toVector.foldLeft(M.empty)(M.combine))
    }

    def parallelFoldMapC[F[_], B](f: A => B)(implicit M: Monad[F],
                                             T: Traverse[Vector],
                                             VF: Foldable[Vector],
                                             MB: Monoid[B]): F[B] = {
      val batchSize = (values.size / cpuCount) + 1
      T.sequence(
          values
            .grouped(batchSize)
            .map(batch => M.pure(batch.foldMap(f)))
            .toVector)
        .map(MB.combineAll)
    }
  }
}
