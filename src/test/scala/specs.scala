import cats.instances.boolean._
import cats.tests._
import cats.kernel.laws.discipline.MonoidTests
import org.scalatest._
import org.typelevel.discipline.scalatest.Discipline

trait CatsSuiteWithoutInstances
  extends FunSuite
    with Matchers
    with Discipline
    with TestSettings
    with StrictCatsEquality

class PrintableSpec extends FlatSpec with Matchers {
  import Printable._
  import PrintableSyntax._
  import PrintableInstances._

  "A Printable" should "return the same value for string" in {
    Printable.format("abc") should be ("abc")
  }

  it should "return string representation of an int" in {
    1.format should be ("1")
  }

  it should "return string representation of a cat" in {
    Cat("Garfield", 3, "orange").format should be ("Garfield is a 3 year-old orange cat.")
  }

  it should "return string representation of Box" in {
    Box(10).format should be ("10")
    Box("hello world").format should be ("hello world")
    Box(true).format should be ("yes")
  }
}

class CatSpec extends FlatSpec with Matchers {
  import cats.Eq
  import cats.syntax.all._

  val cat1 = Cat("Garfield", 3, "orange and black")
  val cat2 = Cat("Heathcliff", 4, "white")

  "Show" should "return string representation of a cat" in {
    cat1.show should be ("Garfield is a 3 year-old orange and black cat.")
  }

  "Eq" should "return true if cats are equal" in {
    assert(Eq.eqv(cat1, cat1))
  }

  it should "return false if cats are different" in {
    assert(!Eq.eqv(cat1, cat2))
  }
}

class BooleanAndMonoidLawSpec extends CatsSuiteWithoutInstances {
  import MonoidAndInstances._
  checkAll("MonoidLaws", MonoidTests[Boolean].monoid)
}

class BooleanOrMonoidLawSpec extends CatsSuiteWithoutInstances {
  import MonoidOrInstances._
  checkAll("MonoidLaws", MonoidTests[Boolean].monoid)
}

class BooleanXorMonoidLawSpec extends CatsSuiteWithoutInstances {
  import MonoidXorInstances._
  checkAll("MonoidLaws", MonoidTests[Boolean].monoid)
}

class BooleanXnorMonoidLawSpec extends CatsSuiteWithoutInstances {
  import MonoidXnorInstances._
  checkAll("MonoidLaws[Boolean]", MonoidTests[Boolean].monoid)
}

class SetMonoidLawsSpec extends CatsSuiteWithoutInstances {
  import cats.kernel.PartialOrder
  import cats.kernel.instances.SetPartialOrder


  implicit def catsKernelStdPartialOrderForSet[A]: PartialOrder[Set[A]] =
    new SetPartialOrder[A]

  {
    import SetUnionMonoidInstances._
    checkAll("SetUnionMonoidLaws[Set[Int]]", MonoidTests[Set[Int]].monoid)
    checkAll("SetUnionMonoidLaws[Set[String]]", MonoidTests[Set[String]].monoid)
  }

  {
    import SetSymDiffMonoidInstances._
    checkAll("SetSymDiffMonoidLaws[Set[Int]]", MonoidTests[Set[Int]].monoid)
    checkAll("SetSymDiffMonoidLaws[Set[String]]", MonoidTests[Set[String]].monoid)
  }
}

class CodecSpecs extends FlatSpec with Matchers {
  "Codec" should "encode/decode ints" in {
    Codec[Int].encode(42) should be ("42")
    Codec[Int].decode("42") should be (42)
  }
}