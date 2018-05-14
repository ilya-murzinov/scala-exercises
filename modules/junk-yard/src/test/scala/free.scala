import cats.Eq
import cats.laws.discipline.MonadTests
import common.test.CatsLawsSpec
import free.Free

class FreeStackUnsafeMonadLawsSpec extends CatsLawsSpec with FreeImplicits {
  import free._
  import cats.instances.all._
  import GenFree._

//  checkAll("StackUnsafeMonad[Free]",
//           MonadTests[Free[Option, ?]].stackUnsafeMonad[String, Int, Boolean])
}

trait FreeImplicits {
  implicit def freeEq[A]: Eq[Free[Option, A]] = ???
}