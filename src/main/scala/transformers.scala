import cats.data.{EitherT, ReaderT}
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object transformers {
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] =
    EitherT.fromOption(powerLevels.get(autobot), s"$autobot unreachable")

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      level1 <- getPowerLevel(ally1)
      level2 <- getPowerLevel(ally2)
    } yield level1 + level2 > 15

  def tacticalReport(ally1: String, ally2: String): String =
    Await.result(canSpecialMove(ally1, ally2).value, 5.seconds) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(b) =>
        if (!b) s"$ally1 and $ally2 need a recharge."
        else s"$ally1 and $ally2 are ready to roll out!"
    }
}

object transformers2 {
  type Levels = Map[String, Int]
  type Response[A] = ReaderT[EitherT[Future, String, ?], Levels, A]

  def getPowerLevel(autobot: String): Response[Int] =
    ReaderT.apply(db => EitherT.fromOption(db.get(autobot), s"$autobot unreachable"))

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      level1 <- getPowerLevel(ally1)
      level2 <- getPowerLevel(ally2)
    } yield level1 + level2 > 15

  def tacticalReport(levels: Levels)(ally1: String, ally2: String): String =
    Await.result(canSpecialMove(ally1, ally2).run(levels).value, 5.seconds) match {
      case Left(msg) => s"Comms error: $msg"
      case Right(b) =>
        if (!b) s"$ally1 and $ally2 need a recharge."
        else s"$ally1 and $ally2 are ready to roll out!"
    }
}