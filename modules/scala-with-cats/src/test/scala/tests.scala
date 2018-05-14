import cats.data.NonEmptyList
import cats.data.Validated.{Invalid, Valid}
import common.test.Test

class FactorialTest extends Test {
  import fact._

  import scala.concurrent.ExecutionContext.Implicits.global
  import scala.concurrent._
  import scala.concurrent.duration._

  "factorialW" should "collect logs using Writer" in {
    val fs = Await.result(Future.sequence(
                            List(
                              Future(factorialW(4)),
                              Future(factorialW(4))
                            )),
                          5.seconds)

    fs(0).run shouldBe (List("fact 0 is 1",
                             "fact 1 is 1",
                             "fact 2 is 2",
                             "fact 3 is 6",
                             "fact 4 is 24"),
    24)

    fs(0) shouldBe fs(1)
  }

  "factorial" should "fuck up" in {
    import java.io.{ByteArrayOutputStream, PrintStream}
    import java.nio.charset.StandardCharsets.UTF_8

    val b = new ByteArrayOutputStream()
    val out = new PrintStream(b)

    Await.result(Future.sequence(
                   Vector(
                     Future(factorial(3, out)),
                     Future(factorial(3, out))
                   )),
                 5.seconds)

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
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
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

class UptimeTest extends Test {
  import uptime._

  "UptimeService" should "return total uptime" in {
    val hosts = Map("host1" -> 10, "host2" -> 6)
    val client = new TestUptimeClient(hosts)
    val service = new UptimeService(client)
    val actual = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    actual shouldBe expected
  }
}

class CheckTest extends Test {
  import checks._

  "emailCheck" should "check email" in {
    emailCheck("aasd@b.as") shouldBe Valid(Email("aasd@b.as"))
  }

  "createUser" should "create valid user" in {
    createUser("Noel", "noel@underscore.io") shouldBe
    Valid(User("Noel", Email("noel@underscore.io")))
  }

  "createUser" should "not create invalid user" in {
    createUser("", "dave@underscore@io") shouldBe
    Invalid(
      NonEmptyList.of("Must be longer than 3 characters",
                      "Must contain the character @ only once"))
  }
}
