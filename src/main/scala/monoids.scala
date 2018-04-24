import cats.Monoid

object MonoidAndInstances {
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = true
    def combine(x: Boolean, y: Boolean) = x && y
  }
}

object MonoidOrInstances {
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = false
    def combine(x: Boolean, y: Boolean) = x || y
  }
}

object MonoidXorInstances {
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = false
    def combine(x: Boolean, y: Boolean) = (x || y) && !(x && y)
  }
}

object MonoidXnorInstances {
  implicit val booleanMonoid: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = true
    def combine(x: Boolean, y: Boolean) = (x && y) || (!x && !y)
  }
}

object SetMonoidInstances {
  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def empty = Set.empty[A]
    def combine(s1: Set[A], s2: Set[A]) = Set.empty[A]
  }
}