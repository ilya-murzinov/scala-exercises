import cats._
import cats.instances.map._
import cats.syntax.semigroup._
import cats.kernel.BoundedSemilattice

object gcounter {
  final case class IntGCounter(counters: Map[String, Int]) {
    def increment(machine: String, amount: Int): IntGCounter =
      IntGCounter(
        counters + (machine -> (amount + counters.getOrElse(machine, 0))))

    def merge(that: IntGCounter): IntGCounter =
      IntGCounter(counters ++ that.counters.map {
        case (k, v) =>
          (k, v.max(counters.getOrElse(k, 0)))
      })

    def total: Int = counters.values.sum
  }

  object boundedSemilattice {
    implicit val intBoundedSemilattice: BoundedSemilattice[Int] =
      new BoundedSemilattice[Int] {
        def empty: Int = 0

        def combine(x: Int, y: Int): Int = x max y
      }

    implicit def setBoundedSemilattice[A]: BoundedSemilattice[Set[A]] =
      new BoundedSemilattice[Set[A]] {
        def empty: Set[A] = Set.empty[A]

        def combine(x: Set[A], y: Set[A]): Set[A] = x union y
      }
  }

  final case class MapGCounter[K, V](counters: Map[K, V]) {
    def increment(machine: K, amount: V)(
        implicit M: Monoid[V]): MapGCounter[K, V] =
      MapGCounter(
        counters + (machine -> (amount |+| counters.getOrElse(machine,
                                                              M.empty))))

    def merge(that: MapGCounter[K, V])(
        implicit B: BoundedSemilattice[V]): MapGCounter[K, V] =
      MapGCounter(counters |+| that.counters)

    def total(implicit M: Monoid[V]): V = M.combineAll(counters.values.toList)
  }

  trait GCounter[F[_, _], K, V] {
    def increment(f: F[K, V])(machine: K, amount: V)(
        implicit M: Monoid[V]): F[K, V]

    def merge(f2: F[K, V], f1: F[K, V])(
        implicit B: BoundedSemilattice[V]): F[K, V]

    def total(f: F[K, V])(implicit M: Monoid[V]): V
  }

  object GCounter {
    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]) = counter
  }

  object map {
    implicit def mapGCounter[K, V]: GCounter[Map, K, V] =
      new GCounter[Map, K, V] {
        def increment(f: Map[K, V])(machine: K, amount: V)(
            implicit M: Monoid[V]): Map[K, V] =
          new MapGCounter[K, V](f).increment(machine, amount).counters

        def merge(f2: Map[K, V], f1: Map[K, V])(
            implicit B: BoundedSemilattice[V]): Map[K, V] =
          new MapGCounter[K, V](f1).merge(new MapGCounter[K, V](f2)).counters

        def total(f: Map[K, V])(implicit M: Monoid[V]): V = MapGCounter(f).total
      }
  }

  trait KeyValueStore[F[_, _]] {
    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
    def get[K, V](f: F[K, V])(k: K): Option[V]
    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)
    def values[K, V](f: F[K, V]): List[V]
  }

  implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
    def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
      kvs.put(f)(key, value)

    def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] = kvs.get(f)(key)

    def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
      kvs.getOrElse(f)(key, default)

    def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
  }

  implicit val mapKVStore: KeyValueStore[Map] = new KeyValueStore[Map] {
    def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)

    def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    def values[K, V](f: Map[K, V]): List[V] = f.values.toList
  }

  implicit def gcounterInstance[F[_, _], K, V](
      implicit KVS: KeyValueStore[F],
      M: Monoid[F[K, V]]): GCounter[F, K, V] =
    new GCounter[F, K, V] {
      def increment(f: F[K, V])(key: K, value: V)(
          implicit MV: Monoid[V]): F[K, V] =
        f.put(key, f.getOrElse(key, MV.empty) |+| value)

      def merge(f1: F[K, V], f2: F[K, V])(
          implicit B: BoundedSemilattice[V]): F[K, V] =
        f1 |+| f2

      def total(f: F[K, V])(implicit M: Monoid[V]): V = M.combineAll(f.values)
    }
}
