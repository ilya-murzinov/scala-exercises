import cats.data.Validated.{Invalid, Valid}
import cats.instances.boolean._
import cats.tests._
import cats.kernel.laws.discipline._
import cats.laws.discipline._
import org.scalatest._
import org.scalatest.prop._
import org.typelevel.discipline.scalatest.Discipline

trait Spec extends PropSpec with PropertyChecks with Matchers

trait Test extends FlatSpec with Matchers

trait CatsLawsSpec
  extends FunSuite
    with Matchers
    with Discipline
    with TestSettings
    with StrictCatsEquality

class PrintableSpec extends Spec {
  import PrintableSyntax._
  import PrintableInstances._
  import GenCat._
  import GenBox._

  property("Printable for String should return the same string") {
    forAll { s: String => Printable.format(s) shouldBe s }
  }

  property("Printable for Int should return string representation of an integer") {
    forAll { i: Int => i.format shouldBe i.toString }
  }

  property("Printable for Cat should return string representation of a cat") {
    forAll { c: Cat => c.format shouldBe s"${c.name} is a ${c.age} year-old ${c.color} cat." }
  }

  property("Printable for Box should return string representation of boxed value") {
    forAll { b: Box[String] => b.format shouldBe Printable.format(b.value) }
    forAll { b: Box[Cat] => b.format shouldBe b.value.format }
    forAll { b: Box[Int] => b.format shouldBe b.value.format }
  }
}

class CatSpec extends Spec {
  import cats.Eq
  import cats.syntax.all._
  import GenCat._

  property("Cat.show should return string representation of a cat") {
    forAll { c: Cat => c.show shouldBe s"${c.name} is a ${c.age} year-old ${c.color} cat." }
  }

  property("Eq for Cat should work") { forAll { (c1: Cat, c2: Cat) =>
    Eq.eqv(c1, c2) shouldBe (c1.name == c2.name && c1.age == c2.age && c1.color == c2.color) }
  }
}

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

class CodecSpec extends Spec {
  property("Codec[Int] should encode ints") {
    forAll { i: Int => Codec[Int].encode(i) shouldBe i.toString }
  }

  property("Codec[Int] should decode ints") {
    forAll { i: Int => Codec[Int].decode(i.toString) shouldBe i }
  }
}

class FactorialSpec extends Test {
  import fact._
  import scala.concurrent._
  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent.duration._

  "factorialW" should "collect logs using Writer" in {
    val fs = Await.result(Future.sequence(List(
      Future(factorialW(4)),
      Future(factorialW(4))
    )), 5.seconds)

    fs(0).run shouldBe (
      List(
        "fact 0 is 1",
        "fact 1 is 1",
        "fact 2 is 2",
        "fact 3 is 6",
        "fact 4 is 24"),
      24)

    fs(0) shouldBe fs(1)
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

    val log = new String(b.toByteArray, UTF_8)

    log should be
      """fact 0 1
        | fact 0 1
        | fact 1 1
        | fact 1 1
        | fact 2 2
        | fact 2 2
        | fact 3 6
        | fact 3 6""".stripMargin
  }
}

class DbReaderSpec extends Test {
  import reader._
  val db = Db(Map(123 -> "name"), Map("name" -> "psswd"))

  "checkPassword" should "succeed if password is correct" in {
    checkLogin(123, "psswd").run(db) shouldBe true
  }

  it should "fail if is not present" in {
    checkLogin(42, "psswd").run(db) shouldBe false
  }

  it should "fail if password is incorrect" in {
    checkLogin(123, "psswd123").run(db) shouldBe false
  }
}

class CalcSpec extends Spec {
  import state._

  property("evalOne should put number to stack") {
    forAll { i: Int => evalOne(i.toString).run(Nil).value shouldBe (List(i), i) }
  }

  property("evalOne should perform '+'") {
    forAll { (i1: Int, i2: Int) =>
      evalOne("+").run(List(i1, i2)).value shouldBe(List(i1 + i2), i1 + i2)
    }
  }

  property("evalOne should perform '-'") {
    forAll { (i1: Int, i2: Int) =>
      evalOne("-").run(List(i1, i2)).value shouldBe(List(i1 - i2), i1 - i2)
    }
  }

  property("evalOne should perform '*'") {
    forAll { (i1: Int, i2: Int) =>
      evalOne("*").run(List(i1, i2)).value shouldBe(List(i1 * i2), i1 * i2)
    }
  }

  property("evalOne should perform '/' if second operand is not 0") {
    forAll { (i1: Int, i2: Int) =>
      if (i2 != 0)
        evalOne("/").run(List(i1, i2)).value shouldBe (List(i1 / i2), i1 / i2)
    }
  }

  property("evalAll should put all numbers to stack") {
    forAll { (stack: List[Int], l: List[Int]) =>
      evalAll(l.map(_.toString)).runS(stack).value shouldBe l.reverse ++ stack
    }
  }

  property("evalAll should perform '+' over a list") {
    forAll { stack: List[Int] =>
      val res = stack.sum
      if (stack.size >= 2)
        evalAll((1 until stack.size).map(_ => "+").toList).run(stack).value shouldBe (List(res), res)
    }
  }

  property("evalAll should perform '-' over a list") {
    forAll { stack: List[Int] =>
      stack match {
        case op1 :: op2 :: tail =>
          val res = (op2 :: tail).fold(op1)(_ - _)
          evalAll((1 until stack.size).map(_ => "-").toList).run(stack).value shouldBe(List(res), res)
        case _ =>
      }
    }
  }

  property("evalAll should perform '*' over a list") {
    forAll { stack: List[Int] =>
      val res = stack.product
      if (stack.size >= 2)
        evalAll((1 until stack.size).map(_ => "*").toList).run(stack).value shouldBe (List(res), res)
    }
  }

  property("evalAll should perform '/' over a list") {
    forAll { stack: List[Int] =>
      val filtered = stack.filter(_ != 0)
      filtered match {
        case op1 :: op2 :: tail =>
          val res = (op2 :: tail).fold(op1)(_ / _)
          evalAll((1 until filtered.size).map(_ => "/").toList).run(filtered).value shouldBe(List(res), res)
        case _ =>
      }
    }
  }
}

class CalcTest extends Test {
  import state._

  "evalAll" should "evaluate expressions" in {
    val program = for {
      _ <- evalAll(List("1", "2", "+"))
      _ <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans

    program.runA(Nil).value shouldBe 21
  }

  "evalInput" should "evaluate expressions" in {
    evalInput("1 2 + 3 4 + *").runA(Nil).value shouldBe 21
  }
}

class IdMonadLawsSpec extends CatsLawsSpec {
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

class TreeStackUnsafeTailRecMSpec extends Spec with StackUnsafeTreeMonad {
  import cats.implicits._
  import GenTree._

  property("stack unsafe tailRecM should be consistent with map") {
    forAll { tree: Tree[Int] =>
      tree.tailRecM[Tree, String](_.map(i => Right(i.toString))) shouldBe tree.map(_.toString)
    }
  }

  property("stack unsafe tailRecM should be consistent with flatMap") {
    forAll { tree: Tree[Int] =>
      val transform: Int => Tree[Either[Tree[Int], String]] =
        (i: Int) => Branch(Leaf(Right(i.toString)), Leaf(Right(i.toString)))

      tree.tailRecM[Tree, String](t => t.flatMap(transform)) shouldBe
        tree.flatMap(i => Branch(Leaf(i.toString), Leaf(i.toString)))
    }
  }
}

//class TreeStackSafeTailRecMTest extends Test with StackSafeTreeMonad {
//  import cats.implicits._
//
//  "stack safe tailRecM" should "work" in {
//    val tree: Tree[Int] = Branch(Branch(Leaf(31), Branch(Leaf(11), Leaf(2))), Leaf(12))
//    tree.tailRecM[Tree, String](_.map(i => Right(i.toString))) shouldBe tree.map(_.toString)
//  }
//}

//class TreeStackSafeTailRecMSpec extends Spec with StackSafeTreeMonad {
//  import cats.implicits._
//  import GenTree._
//
//  property("stack safe tailRecM should be consistent with map") {
//    forAll { tree: Tree[Int] =>
//      tree.tailRecM[Tree, String](_.map(i => Right(i.toString))) shouldBe tree.map(_.toString)
//    }
//  }
//
//  property("stack safe tailRecM should be consistent with flatMap") {
//    forAll { tree: Tree[Int] =>
//      val transform: Int => Tree[Either[Tree[Int], String]] =
//        (i: Int) => Branch(Leaf(Right(i.toString)), Leaf(Right(i.toString)))
//
//      tree.tailRecM[Tree, String](t => t.flatMap(transform)) shouldBe
//        tree.flatMap(i => Branch(Leaf(i.toString), Leaf(i.toString)))
//    }
//  }
//}

class TreeStackUnsafeMonadLawsSpec extends CatsLawsSpec with StackUnsafeTreeMonad {
  import cats.instances.all._
  import GenTree._

  checkAll("StackUnsafeMonad[Tree]", MonadTests[Tree].stackUnsafeMonad[String, Int, Boolean])
}

//class TreeStackSafeMonadLawsSpec extends CatsLawsSpec with StackSafeTreeMonad {
//  import cats.instances.all._
//  import GenTree._
//
//  checkAll("StackSafeMonad[Tree]", MonadTests[Tree].monad[String, Int, Boolean])
//}

class TransformersTest extends Test {
  import transformers._

  "tacticalReport" should "work" in {
    tacticalReport("Jazz", "Bumblebee") shouldBe "Jazz and Bumblebee need a recharge."
    tacticalReport("Bumblebee", "Hot Rod") shouldBe "Bumblebee and Hot Rod are ready to roll out!"
    tacticalReport("Jazz", "Ironhide") shouldBe "Comms error: Ironhide unreachable"
  }
}

class Transformers2Test extends Test {
  import transformers2._

  val powerLevels: Levels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  "tacticalReport" should "work" in {
    tacticalReport(powerLevels)("Jazz", "Bumblebee") shouldBe "Jazz and Bumblebee need a recharge."
    tacticalReport(powerLevels)("Bumblebee", "Hot Rod") shouldBe "Bumblebee and Hot Rod are ready to roll out!"
    tacticalReport(powerLevels)("Jazz", "Ironhide") shouldBe "Comms error: Ironhide unreachable"
  }
}

class ReadUserTest extends Test {
  import validated._

  "readUser" should "parse correct user" in {
    readUser(Map("name" -> "John Doe", "age" -> "42")) shouldBe
      Valid(User("John Doe", 42))
  }

  "readUser" should "return all errors" in {
    readUser(Map()) shouldBe
      Invalid(List("name must be present", "age must be present"))

    readUser(Map("name" -> "", "age" -> "-42")) shouldBe
      Invalid(List("name must be non-blank", "age must be positive"))

    readUser(Map("name" -> "Jim")) shouldBe
      Invalid(List("age must be present"))
  }
}