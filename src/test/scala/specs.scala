import cats.tests.CatsSuite
import cats.kernel.laws.discipline.MonoidTests
import org.scalatest._

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

class BooleanAndMonoidLawSpec extends CatsSuite {
  import MonoidAndInstances._
  checkAll("MonoidLaws", MonoidTests[Boolean].monoid)
}

class BooleanOrMonoidLawSpec extends CatsSuite {
  import MonoidOrInstances._
  checkAll("MonoidLaws", MonoidTests[Boolean].monoid)
}

class BooleanXorMonoidLawSpec extends CatsSuite {
  import MonoidXorInstances._
  checkAll("MonoidLaws", MonoidTests[Boolean].monoid)
}

class BooleanXnorMonoidLawSpec extends CatsSuite {
  import MonoidXnorInstances._
  checkAll("MonoidLaws[Boolean]", MonoidTests[Boolean].monoid)
}

class SetMonoidLawsSpec extends CatsSuite {
  import cats.Monoid
  import cats.kernel.instances.{_ => _}
  import SetMonoidInstances._
  println(implicitly[Monoid[Set[Int]]])
  checkAll("MonoidLaws[Set[Int]]", MonoidTests[Set[Int]].monoid)
  checkAll("MonoidLaws[Set[String]]", MonoidTests[Set[String]].monoid)
}