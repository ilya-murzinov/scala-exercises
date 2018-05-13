import java.io.PrintStream

import cats._
import cats.data._
import cats.implicits._

object fact {
  def slowly[A](body: => A) =
    try body
    finally Thread.sleep(100)

  def factorial(n: Int, out: PrintStream): Int = {
    val ans = slowly(if (n == 0) 1 else n * factorial(n - 1, out))
    out.println(s"fact $n $ans")
    ans
  }

  type Logged[A] = Writer[List[String], A]

  def factorialW(n: Int): Logged[Int] =
    for {
      ans <- if (n == 0) 1.pure[Logged]
      else slowly(factorialW(n - 1).map(_ * n))
      _ <- List(s"fact $n is $ans").tell
    } yield ans
}
