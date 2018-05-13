import cats._
import cats.instances.all._
import cats.syntax.all._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object uptime {
  trait UptimeClient[F[_]] {
    def getUptime(hostname: String): F[Int]
  }

  class UptimeService[F[_]](client: UptimeClient[F])(implicit A: Applicative[F]) {
    def getTotalUptime(hostnames: List[String]): F[Int] =
      hostnames.traverse(client.getUptime).map(_.sum)
  }

  class RealUptimeClient extends UptimeClient[Future] {
    def getUptime(hostname: String) = ???
  }

  class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] { 
    def getUptime(hostname: String): Id[Int] = hosts.getOrElse(hostname, 0)
  }
}