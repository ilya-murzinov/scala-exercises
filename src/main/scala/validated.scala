import cats.data.Validated
import cats.instances.all._
import cats.syntax.all._

import scala.util.Try

object validated {
  type Fields = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  case class User(name: String, age: Int)

  def getValue(name: String)(fields: Fields): FailFast[String] =
    fields.get(name).toRight(List(s"$name must be present"))

  def parseInt(name: String)(s: String): FailFast[Int] =
    Try(s.toInt).toOption.toRight(List(s"$name must be integer"))

  def nonBlank(name: String)(s: String): FailFast[String] =
    if (!s.isEmpty) s.asRight
    else List(s"$name must be non-blank").asLeft

  def nonNegative(name: String)(int: Int): FailFast[Int] =
    if (int > 0) int.asRight
    else List(s"$name must be positive").asLeft

  def readName(fields: Fields): FailFast[String] =
    for {
      v <- getValue("name")(fields)
      name <- nonBlank("name")(v)
    } yield name

  def readAge(fields: Fields): FailFast[Int] =
    for {
      v <- getValue("age")(fields)
      i <- parseInt("age")(v)
      age <- nonNegative("age")(i)
    } yield age

  def readUser(fields: Fields): FailSlow[User] =
    (readName(fields).toValidated, readAge(fields).toValidated).mapN(User.apply)
}