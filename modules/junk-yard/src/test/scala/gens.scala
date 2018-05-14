import free._
import org.scalacheck._

object GenFree {
  implicit def arbFree1[A]: Arbitrary[Free[Option, A]] = ???
  implicit def arbFree2[A, B]: Arbitrary[Free[Option, A => B]] = ???
}