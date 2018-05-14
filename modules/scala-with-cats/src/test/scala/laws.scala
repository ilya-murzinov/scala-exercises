import cats.kernel.instances.SetPartialOrder
import cats.kernel.laws.discipline.{BoundedSemilatticeTests, MonoidTests}
import cats.laws.discipline.{FunctorTests, MonadTests}
import cats.instances.boolean._
import common.test.CatsLawsSpec

class BooleanAndMonoidLawSpec extends CatsLawsSpec {
  import MonoidAndInstances._
  checkAll("AndMonoid[Boolean]", MonoidTests[Boolean].monoid)
}

class BooleanOrMonoidLawSpec extends CatsLawsSpec {
  import MonoidOrInstances._
  checkAll("OrMonoid[Boolean]", MonoidTests[Boolean].monoid)
}

class BooleanXorMonoidLawSpec extends CatsLawsSpec {
  import MonoidXorInstances._
  checkAll("XorMonoid[Boolean]", MonoidTests[Boolean].monoid)
}

class BooleanXnorMonoidLawSpec extends CatsLawsSpec {
  import MonoidXnorInstances._
  checkAll("XnorMonoid[Boolean]", MonoidTests[Boolean].monoid)
}

class SetMonoidLawsSpec extends CatsLawsSpec {
  import cats.kernel.PartialOrder
  import cats.kernel.instances.SetPartialOrder

  implicit def catsKernelStdPartialOrderForSet[A]: PartialOrder[Set[A]] =
    new SetPartialOrder[A]

  {
    import SetUnionMonoidInstances._
    checkAll("UnionMonoid[Set[Int]]", MonoidTests[Set[Int]].monoid)
    checkAll("UnionMonoid[Set[String]]", MonoidTests[Set[String]].monoid)
  }

  {
    import SetSymDiffMonoidInstances._
    checkAll("SymDiffMonoid[Set[Int]]", MonoidTests[Set[Int]].monoid)
    checkAll("SymDiffMonoid[Set[String]]", MonoidTests[Set[String]].monoid)
  }
}

class TreeFunctorLawsSpec extends CatsLawsSpec {
  import cats.instances.all._
  import GenTree._
  import Tree._

  checkAll("Functor[Tree]", FunctorTests[Tree].functor[String, Int, Boolean])
}

class IdMonadLawsSpec extends CatsLawsSpec {
  import cats.Id
  import cats.laws.discipline.SemigroupalTests.Isomorphisms
  import cats.instances.all._
  import id._

  implicit val iso: Isomorphisms[Id] = Isomorphisms.invariant[Id]

  checkAll("Monad[Id]", MonadTests[Id].monad[String, Int, Boolean])
}

class TreeStackUnsafeMonadLawsSpec
    extends CatsLawsSpec
    with StackUnsafeTreeMonad {
  import cats.instances.all._
  import GenTree._

  checkAll("StackUnsafeMonad[Tree]",
           MonadTests[Tree].stackUnsafeMonad[String, Int, Boolean])
}

class TreeStackSafeMonadLawsSpec extends CatsLawsSpec with StackSafeTreeMonad {
  import cats.instances.all._
  import GenTree._

  //checkAll("StackSafeMonad[Tree]", MonadTests[Tree].monad[String, Int, Boolean])
}

class IntBoundedSemilatticeLawsSpec extends CatsLawsSpec {
  import gcounter.boundedSemilattice._
  import cats.kernel.instances.int._
  import org.scalacheck._

  implicit val arbInt: Arbitrary[Int] = Arbitrary(Gen.posNum[Int])
  implicit val order: SetPartialOrder[Int] = new SetPartialOrder[Int]

  checkAll("BoundedSemilattice[Int]",
           BoundedSemilatticeTests[Int].boundedSemilattice)
  checkAll("BoundedSemilattice[Set]",
           BoundedSemilatticeTests[Set[Int]].boundedSemilattice)
}
