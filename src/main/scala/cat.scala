import cats._

final case class Cat(name: String, age: Int, color: String)

object Cat {
  implicit val catPrintable: Printable[Cat] =
    (a: Cat) => s"${a.name} is a ${a.age} year-old ${a.color} cat."

  implicit val catShow: Show[Cat] =
    Show.show(a => s"${a.name} is a ${a.age} year-old ${a.color} cat.")

  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat]((c1, c2) =>
      c1.name == c2.name && c1.age == c2.age && c1.color == c2.color)
}
