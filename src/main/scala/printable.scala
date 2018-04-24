trait Printable[A] {
  def format(a: A): String
}

object Printable {
  def apply[A](implicit P: Printable[A]) = P

  def format[A](a: A)(implicit P: Printable[A]) = P.format(a)
  def print[A](a: A)(implicit P: Printable[A]) = println(P.format(a))
}

object PrintableSyntax {
  implicit class PrintableOps[A](a: A) {
    def format(implicit P: Printable[A]) = P.format(a)
    def print(implicit P: Printable[A]) = println(P.format(a))
  }
}

object PrintableInstances {
  implicit val stringPrintable = new Printable[String] {
    def format(a: String) = a
  }
  implicit val intPrintable = new Printable[Int] {
    def format(a: Int) = a.toString
  }
}