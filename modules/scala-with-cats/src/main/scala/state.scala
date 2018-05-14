import cats._
import cats.data._
import cats.syntax.applicative._

import scala.util.Try

object state {
  type CalcState[A] = State[List[Int], A]

  def func(sym: String): (Int, Int) => Int =
    sym match {
      case "+" => _ + _
      case "-" => _ - _
      case "*" => _ * _
      case "/" => _ / _
    }

  def evalOne(sym: String): CalcState[Int] = {
    val maybeInt = Try(sym.toInt).toOption
    State {
      case l if maybeInt.isDefined =>
        val int = maybeInt.get
        (int +: l, int)
      case op1 :: op2 :: tail =>
        def res = func(sym)(op1, op2)
        (res +: tail, res)
    }
  }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (s1, s2) =>
      s1.flatMap(_ => evalOne(s2))
    }

  def evalInput(input: String): CalcState[Int] =
    evalAll(input.split("\\s").toList)
}
