import cats.instances.boolean._
import cats.tests._
import cats.kernel.laws.discipline.MonoidTests
import cats.laws.discipline._
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
  checkAll("AndMonoid[Boolean]", MonoidTests[Boolean].monoid)
}

class BooleanOrMonoidLawSpec extends CatsSuiteWithoutInstances {
  import MonoidOrInstances._
  checkAll("OrMonoid[Boolean]", MonoidTests[Boolean].monoid)
}

class BooleanXorMonoidLawSpec extends CatsSuiteWithoutInstances {
  import MonoidXorInstances._
  checkAll("XorMonoid[Boolean]", MonoidTests[Boolean].monoid)
}

class BooleanXnorMonoidLawSpec extends CatsSuiteWithoutInstances {
  import MonoidXnorInstances._
  checkAll("XnorMonoid[Boolean]", MonoidTests[Boolean].monoid)
}

class SetMonoidLawsSpec extends CatsSuiteWithoutInstances {
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

object GenTrees {
  import org.scalacheck._

  def genLeaf[A: Arbitrary]: Gen[Tree[A]] = for {
    e <- Arbitrary.arbitrary[A]
  } yield Leaf(e)

  def genBranch[A: Arbitrary]: Gen[Tree[A]] = for {
    l <- Gen.sized(h => Gen.resize(h/2, genTree[A]))
    r <- Gen.sized(h => Gen.resize(h/2, genTree[A]))
  } yield Branch(l, r)

  def genTree[A: Arbitrary]: Gen[Tree[A]] = Gen.sized { height =>
    if (height <= 0)
      genLeaf[A]
    else
      Gen.oneOf(genBranch[A], genLeaf[A])
  }

  implicit def arbTree[A: Arbitrary]: Arbitrary[Tree[A]] =
    Arbitrary(genTree[A])
}

class TreeFunctorLawsSpec extends CatsSuiteWithoutInstances {
  import cats.instances.all._
  import GenTrees._
  import Tree._

  checkAll("Functor[Tree]", FunctorTests[Tree].functor[String, Int, Boolean])
}

class CodecSpec extends FlatSpec with Matchers {
  "Codec" should "encode/decode ints" in {
    Codec[Int].encode(42) should be ("42")
    Codec[Int].decode("42") should be (42)
  }
}

class FactorialSpec extends FlatSpec with Matchers {
  import cats.data._
  import fact._
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  "factorialW" should "collect logs using Writer" in {
    val fs = Await.result(Future.sequence(List(
      Future(factorialW(4)),
      Future(factorialW(4))
    )), 5.seconds)

    fs(0).run should be ((
      List(
        "fact 0 is 1",
        "fact 1 is 1",
        "fact 2 is 2",
        "fact 3 is 6",
        "fact 4 is 24"), 
      24))

    fs(0) should be (fs(1))
  }

  "factorial" should "fuck up" in {
    import java.io.{ ByteArrayOutputStream, PrintStream }
    import java.nio.charset.StandardCharsets.UTF_8
    
    val b = new ByteArrayOutputStream()
    val out = new PrintStream(b)
    
    Await.result(Future.sequence(Vector(
      Future(factorial(3, out)),
      Future(factorial(3, out))
    )), 5.seconds)

    val log = new String(b.toByteArray(), UTF_8)

    log should be 
    ("""fact 0 1
      | fact 0 1
      | fact 1 1
      | fact 1 1
      | fact 2 2
      | fact 2 2
      | fact 3 6
      | fact 3 6""".stripMargin)
  }
}

class IdMonadLawsSpec extends CatsSuiteWithoutInstances {
  import cats.Id
  import cats.laws._
  import cats.laws.discipline.SemigroupalTests.Isomorphisms
  import cats.instances.all._
  import id._

  // TODO: WTF???
  implicit val iso: Isomorphisms[Id] = new Isomorphisms[Id] {
    override def associativity[A, B, C](fs: ((A, (B, C)), ((A, B), C))): IsEq[(A, B, C)] =
     fs match { case ((a1, (b1, c1)), ((a2, b2), c2)) => (a1, b1, c1) <-> (a2, b2, c2) }

    override def leftIdentity[A](fs: ((Unit, A), Id[A])): IsEq[Id[A]] = 
      fs match { case (((), a), b) => a <-> b }

    override def rightIdentity[A](fs: ((A, Unit), Id[A])): IsEq[Id[A]] =
      fs match { case ((a, ()), b) => a <-> b }
  }

  checkAll("Monad[Id]", MonadTests[Id].monad[String, Int, Boolean])
}

class TreeStackUnsafeMonadLawsSpec extends CatsSuiteWithoutInstances with StackUnsafeTreeMonad {
  import cats.instances.all._
  import GenTrees._

  checkAll("StackUnsafeMonad[Tree]", MonadTests[Tree].stackUnsafeMonad[String, Int, Boolean])
}

//class TreeStackSafeMonadLawsSpec extends CatsSuiteWithoutInstances {
//  import cats.instances.all._
//  import GenTrees._
//  import Tree._
//
//  checkAll("StackSafeMonad[Tree]", MonadTests[Tree].monad[String, Int, Boolean])
//}