import cats.Monoid

object MonoidAndInstances {
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = true
    def combine(x: Boolean, y: Boolean): Boolean = x && y
  }
}

object MonoidOrInstances {
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = false
    def combine(x: Boolean, y: Boolean): Boolean = x || y
  }
}

object MonoidXorInstances {
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = false
    def combine(x: Boolean, y: Boolean): Boolean = (x || y) && !(x && y)
  }
}

object MonoidXnorInstances {
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = true
    def combine(x: Boolean, y: Boolean): Boolean = (x && y) || (!x && !y)
  }
}

object SetUnionMonoidInstances {
  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty = Set.empty[A]
    def combine(s1: Set[A], s2: Set[A]): Set[A] = s1 union s2
  }
}

object SetSymDiffMonoidInstances {
  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty = Set.empty[A]
    def combine(s1: Set[A], s2: Set[A]): Set[A] = (s1 diff s2) union (s2 diff s1)
  }
}

object SuperAdder {
  def add[A: Monoid](items: List[A]): A = items.fold(Monoid[A].empty)(Monoid[A].combine)
}

case class Order(totalCost: Double, quantity: Double)
object Order {
  implicit val orderMonoid: Monoid[Order] = new Monoid[Order] {
    def empty = Order(0, 0)
    def combine(o1: Order, o2: Order) = Order(o1.totalCost + o2.totalCost, o1.quantity + o2.quantity)
  }
}