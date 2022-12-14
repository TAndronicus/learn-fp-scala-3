package free_monad

import domain.State
import functor.{Functor, StateFunctorInstance}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class FreeMonadTest extends AnyWordSpecLike with Matchers:

  import Free.{fold, lift, given}
  import monad.OptionMonadInstance.given

  "state functor" should {
    "map value" in {
      sealed trait KVStore[V]
      case class Put[V](k: String, v: V) extends KVStore[V]
      case class Get[V](k: String) extends KVStore[V]
      case class Delete(k: String) extends KVStore[Unit]

      def put[V](k: String, v: V): Free[KVStore, V] = lift(Put(k, v))

      def get[V](k: String): Free[KVStore, V] = lift(Get(k))

      def delete(k: String): Free[KVStore, Unit] = lift(Delete(k))

      val resSome = for {
        _ <- put("a", 2)
        a1: Int <- get("a")
        _ <- put("a", 3)
        a2: Int <- get("a")
        _ <- put("b", 5)
        b: Int <- get("b")
      } yield a1 + a2 + b

      val resNone = for {
        _ <- put("a", 2)
        a1: Int <- get("a")
        _ <- put("a", 3)
        a2: Int <- get("a")
        b: Int <- get("b")
      } yield a1 + a2 + b

      def natural: Natural[KVStore, Option] = new Natural[KVStore, Option]:

        import scala.collection.mutable

        private val m = mutable.HashMap[String, Any]()

        override def transform[A](fa: KVStore[A]): Option[A] = fa match
          case Put(k, v) =>
            m.put(k, v)
            Some(v)
          case Get(k) =>
            m.get(k).map(_.asInstanceOf[A])
          case Delete(k) =>
            m.remove(k).map(_.asInstanceOf[A])

      fold(resSome, natural) shouldBe Some(10)
      fold(resNone, natural) shouldBe None
    }
  }
