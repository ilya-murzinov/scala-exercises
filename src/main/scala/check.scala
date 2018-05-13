import cats._
import cats.data._
import cats.data.Validated._
import cats.instances.all._
import cats.syntax.all._

object predicate {
  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] = NonEmptyList.of(s)

  trait Predicate[E, A] {
    def apply(value: A)(implicit SE: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(f) => f(value)
        case And(left, right) =>
          (left(value), right(value)).mapN((_, _) => value)
        case Or(left, right) =>
          (left(value), right(value)) match {
            case (Invalid(l), Invalid(r)) => (l |+| r).invalid
            case _ => value.valid
          }
      }

    def and(that: Predicate[E, A]): And[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Or[E, A] = Or(this, that)

    def run(implicit SE: Semigroup[E]): A => Either[E, A] = a => apply(a).toEither
  }

  object Predicate {
    def lift[A](errors: Errors, pred: A => Boolean): Predicate[Errors, A] =
      Pure(a => if (pred(a)) Valid(a) else Invalid(errors))
  }

  case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]
  case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.length > n)

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      str => str.forall(_.isLetterOrDigit))

  def contains(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character $char"),
    str => str.contains(char))

  def containsOnce(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character $char only once"),
    str => str.count(c => c == char) == 1)
}

object check {
  import predicate._

  sealed trait Check[E, A, B] {
    def apply(value: A)(implicit SE: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C] = Map(this)(f)

    def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap(this)(f)

    def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this)(that)
  }

  object Check {
    def apply[E, A](pred: Predicate[E, A]) = Pure(pred)
  }

  case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(value: A)(implicit SE: Semigroup[E]): Validated[E, A] = pred(value)
  }

  case class Map[E, A, B, C](check: Check[E, A, B])(func: B => C) extends Check[E, A, C] {
    def apply(value: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(value).map(func)
  }

  case class FlatMap[E, A, B, C](check: Check[E, A, B])(func: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(value: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(value).withEither(_.flatMap(b => func(b)(value).toEither))
  }

  case class AndThen[E, A, B, C](check: Check[E, A, B])(that: Check[E, B, C]) extends Check[E, A, C] {
    def apply(value: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(value).withEither(_.flatMap(b => that(b).toEither))
  }
}

case class Email(value: String)
case class User(name: String, email: Email)

object checks {
  import predicate._
  import check._

  val usernameCheck: Check[Errors, String, String] =
    Check(longerThan(3) and alphanumeric)

  val emailCheck: Check[Errors, String, Email] =
    Check(containsOnce('@'))
      .map[(String, String)](s => {
        val Array(left, right) = s.split("@")
        (left, right)
      })
      .andThen[String](Check(predicate.Pure[Errors, (String, String)] {
        case (l, r) =>
          (Check(longerThan(0))(l),
            Check(longerThan(3) and contains('.'))(r)).mapN((_, _))
      })
      .map { case (l, r) => l + "@" + r })
      .map(Email)

  def createUser(username: String, email: String): Validated[Errors, User] =
    (usernameCheck(username), emailCheck(email)).mapN(User)
}

object kleisli {
  import predicate._

  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]

  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] =
    Kleisli[Result, A, A](pred.run)

  val usernameCheckK: Check[String, String] = checkPred(longerThan(3) and alphanumeric)

  val emailCheckK: Check[String, Email] =
    checkPred(containsOnce('@'))
      .map[(String, String)](s => {
      val Array(left, right) = s.split("@")
      (left, right)
    })
    .andThen[String](checkPred(predicate.Pure[Errors, (String, String)] {
      case (l, r) =>
        (checkPred(longerThan(0))(l).toValidated,
          checkPred(longerThan(3) and contains('.'))(r).toValidated).mapN((_, _))
    })
    .map { case (l, r) => l + "@" + r })
    .map(Email)

  def createUserK(username: String, email: String): Validated[Errors, User] =
    (usernameCheckK(username).toValidated, emailCheckK(email).toValidated).mapN(User)
}