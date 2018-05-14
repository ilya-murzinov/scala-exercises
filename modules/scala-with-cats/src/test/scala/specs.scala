import common.test._

import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import cats.instances.boolean._
import cats.kernel.instances.SetPartialOrder
import cats.tests._
import cats.kernel.laws.discipline._
import cats.laws.discipline._
import org.scalatest.{Spec => _, _}
import org.scalatest.prop._
import org.typelevel.discipline.scalatest.Discipline

class PrintableSpec extends Spec {
  import PrintableSyntax._
  import PrintableInstances._
  import GenCat._
  import GenBox._

  property("Printable for String should return the same string") {
    forAll { s: String =>
      Printable.format(s) shouldBe s
    }
  }

  property(
    "Printable for Int should return string representation of an integer") {
    forAll { i: Int =>
      i.format shouldBe i.toString
    }
  }

  property("Printable for Cat should return string representation of a cat") {
    forAll { c: Cat =>
      c.format shouldBe s"${c.name} is a ${c.age} year-old ${c.color} cat."
    }
  }

  property(
    "Printable for Box should return string representation of boxed value") {
    forAll { b: Box[String] =>
      b.format shouldBe Printable.format(b.value)
    }
    forAll { b: Box[Cat] =>
      b.format shouldBe b.value.format
    }
    forAll { b: Box[Int] =>
      b.format shouldBe b.value.format
    }
  }
}

class CatSpec extends Spec {
  import cats.Eq
  import cats.syntax.all._
  import GenCat._

  property("Cat.show should return string representation of a cat") {
    forAll { c: Cat =>
      c.show shouldBe s"${c.name} is a ${c.age} year-old ${c.color} cat."
    }
  }

  property("Eq for Cat should work") {
    forAll { (c1: Cat, c2: Cat) =>
      Eq.eqv(c1, c2) shouldBe (c1.name == c2.name && c1.age == c2.age && c1.color == c2.color)
    }
  }
}

class CodecSpec extends Spec {
  property("Codec[Int] should encode ints") {
    forAll { i: Int =>
      Codec[Int].encode(i) shouldBe i.toString
    }
  }

  property("Codec[Int] should decode ints") {
    forAll { i: Int =>
      Codec[Int].decode(i.toString) shouldBe i
    }
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
    forAll { i: Int =>
      evalOne(i.toString).run(Nil).value shouldBe (List(i), i)
    }
  }

  property("evalOne should perform '+'") {
    forAll { (i1: Int, i2: Int) =>
      evalOne("+").run(List(i1, i2)).value shouldBe (List(i1 + i2), i1 + i2)
    }
  }

  property("evalOne should perform '-'") {
    forAll { (i1: Int, i2: Int) =>
      evalOne("-").run(List(i1, i2)).value shouldBe (List(i1 - i2), i1 - i2)
    }
  }

  property("evalOne should perform '*'") {
    forAll { (i1: Int, i2: Int) =>
      evalOne("*").run(List(i1, i2)).value shouldBe (List(i1 * i2), i1 * i2)
    }
  }

  property("evalOne should perform '/' if second operand is not 0") {
    forAll { (i1: Int, i2: Int) =>
      whenever(i2 != 0) {
        evalOne("/").run(List(i1, i2)).value shouldBe (List(i1 / i2), i1 / i2)
      }
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
      whenever(stack.size >= 2) {
        evalAll((1 until stack.size).map(_ => "+").toList)
          .run(stack)
          .value shouldBe (List(res), res)
      }
    }
  }

  property("evalAll should perform '-' over a list") {
    forAll { stack: List[Int] =>
      stack match {
        case op1 :: op2 :: tail =>
          val res = (op2 :: tail).fold(op1)(_ - _)
          evalAll((1 until stack.size).map(_ => "-").toList)
            .run(stack)
            .value shouldBe (List(res), res)
        case _ =>
      }
    }
  }

  property("evalAll should perform '*' over a list") {
    forAll { stack: List[Int] =>
      val res = stack.product
      whenever(stack.size >= 2) {
        evalAll((1 until stack.size).map(_ => "*").toList)
          .run(stack)
          .value shouldBe (List(res), res)
      }
    }
  }

  property("evalAll should perform '/' over a list") {
    forAll { stack: List[Int] =>
      val filtered = stack.filter(_ != 0)
      filtered match {
        case op1 :: op2 :: tail =>
          val res = (op2 :: tail).fold(op1)(_ / _)
          evalAll((1 until filtered.size).map(_ => "/").toList)
            .run(filtered)
            .value shouldBe (List(res), res)
        case _ =>
      }
    }
  }
}

class TreeStackUnsafeTailRecMSpec extends Spec with StackUnsafeTreeMonad {
  import cats.implicits._
  import GenTree._

  property("stack unsafe tailRecM should be consistent with map") {
    forAll { tree: Tree[Int] =>
      tree.tailRecM[Tree, String](_.map(i => Right(i.toString))) shouldBe tree
        .map(_.toString)
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

class TreeStackSafeTailRecMTest extends Test with StackSafeTreeMonad {
  import cats.implicits._

  "stack safe tailRecM" should "work" ignore {
    val tree: Tree[Int] =
      Branch(Branch(Leaf(31), Branch(Leaf(11), Leaf(2))), Leaf(12))
    tree.tailRecM[Tree, String](_.map(i => Right(i.toString))) shouldBe tree
      .map(_.toString)
  }
}

class TreeStackSafeTailRecMSpec extends Spec with StackSafeTreeMonad {
  import cats.implicits._
  import GenTree._

  ignore("stack safe tailRecM should be consistent with map") {
    forAll { tree: Tree[Int] =>
      tree.tailRecM[Tree, String](_.map(i => Right(i.toString))) shouldBe tree
        .map(_.toString)
    }
  }

  ignore("stack safe tailRecM should be consistent with flatMap") {
    forAll { tree: Tree[Int] =>
      val transform: Int => Tree[Either[Tree[Int], String]] =
        (i: Int) => Branch(Leaf(Right(i.toString)), Leaf(Right(i.toString)))

      tree.tailRecM[Tree, String](t => t.flatMap(transform)) shouldBe
      tree.flatMap(i => Branch(Leaf(i.toString), Leaf(i.toString)))
    }
  }
}

class ListFoldableOpsSpec extends Spec {
  import cats.instances.int._
  import foldable._

  property("mapF should be consistent with map") {
    forAll { l: List[Int] =>
      def f = (i: Int) => i.toString
      l.mapF(f) shouldBe l.map(f)
    }
  }

  property("flatMapF should be consistent with flatMap") {
    forAll { l: List[Int] =>
      def f = (i: Int) => i.toString.map(_.toString).toList
      l.flatMapF(f) shouldBe l.flatMap(f)
    }
  }

  property("filterF should be consistent with filter") {
    forAll { l: List[Int] =>
      def f = (i: Int) => i % 3 == 0
      l.filterF(f) shouldBe l.filter(f)
    }
  }

  property("sumF should be consistent with sum") {
    forAll { l: List[Int] =>
      l.sumF shouldBe l.sum
    }
  }
}

class FoldMapSpec extends Spec {
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global
  import cats.instances.all._
  import mapreduce._

  property("foldMap should be consistent with map and fold") {
    forAll { v: Vector[Int] =>
      v.foldMap(_.toString) shouldBe v.map(_.toString).mkString
    }
  }

  property("parallelFoldMap should be consistent with map and fold") {
    forAll { v: Vector[Int] =>
      Await.result(v.parallelFoldMap(_.toString), 5.seconds) shouldBe
      v.map(_.toString).mkString
    }
  }

  property("parallelFoldMapC should be consistent with map and fold") {
    forAll { v: Vector[Int] =>
      Await.result(v.parallelFoldMapC[Future, String](_.toString), 5.seconds) shouldBe
      v.map(_.toString).mkString
    }
  }
}

class ChecksSpec extends Spec {
  import cats.data.NonEmptyList
  import predicate._
  import checks._

  property("usernameCheck should validate username") {
    forAll { s: String =>
      val res =
        if (s.length > 3 && s.forall(_.isLetterOrDigit))
          Valid(s)
        else if (s.length <= 3 && s.forall(_.isLetterOrDigit))
          Invalid(error("Must be longer than 3 characters"))
        else if (s.length > 3 && !s.forall(_.isLetterOrDigit))
          Invalid(error("Must be all alphanumeric characters"))
        else
          Invalid(
            NonEmptyList.of("Must be longer than 3 characters",
                            "Must be all alphanumeric characters"))

      usernameCheck(s) shouldBe res
    }
  }
}

class KleisliChecksSpec extends Spec {
  import checks._
  import kleisli._

  property("createUserK should be consistent with createUser") {
    forAll { (username: String, email: String) =>
      createUserK(username, email) shouldBe createUser(username, email)
    }
  }
}

class GCounterSpec extends Spec {
  import cats.Eq
  import cats.kernel.instances.map._
  import gcounter._
  import GenMap._

  implicit val intEq: Eq[Int] = (x: Int, y: Int) => x == y

  property("increment should add element if it's not present") {
    import cats.instances.int._

    forAll { (s: String, i: Int) =>
      whenever(i > 0) {
        Eq.eqv(gcounterInstance[Map, String, Int].increment(Map.empty)(s, i),
               Map(s -> i)) shouldBe true
      }
    }
  }

  property("increment should increment element value if it is present") {
    import cats.instances.int._

    forAll { (s: String, i: Int) =>
      whenever(i > 0) {
        Eq.eqv(gcounterInstance[Map, String, Int].increment(Map(s -> 42))(s, i),
               Map(s -> (42 + i))) shouldBe true
      }
    }
  }

  property("merge should merge maps") {
    import boundedSemilattice._

    forAll(genMaps) {
      case (m1: Map[String, Int], m2: Map[String, Int]) =>
        val expected = m1 ++ m2.map {
          case (k, v) =>
            (k, v.max(m1.getOrElse(k, 0)))
        }
        Eq.eqv(gcounterInstance[Map, String, Int].merge(m1, m2), expected) shouldBe true
    }
  }

  property("merge should be idempotent") {
    import boundedSemilattice._

    forAll(genMap) { m: Map[String, Int] =>
      Eq.eqv(gcounterInstance[Map, String, Int].merge(m, m), m) shouldBe true
    }
  }

  property("merge should have left identity") {
    import boundedSemilattice._

    forAll(genMap) { m: Map[String, Int] =>
      Eq.eqv(gcounterInstance[Map, String, Int].merge(Map.empty, m), m) shouldBe true
    }
  }

  property("merge should have right identity") {
    import boundedSemilattice._

    forAll(genMap) { m: Map[String, Int] =>
      Eq.eqv(gcounterInstance[Map, String, Int].merge(m, Map.empty), m) shouldBe true
    }
  }

  property("total should return sum of values") {
    import cats.instances.int._

    forAll { m: Map[String, Int] =>
      Eq.eqv(gcounterInstance[Map, String, Int].total(m), m.values.sum) shouldBe true
    }
  }
}
