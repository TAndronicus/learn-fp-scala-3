package functor

import domain.State
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class StateFunctorTest extends AnyWordSpecLike with Matchers:

  import Functor.given
  import StateFunctorInstance.given

  "state functor" should {
    "map value" in {
      val state: State[String, String] = State.get[String]
      val newState: State[String, Int] = state.map(i => i.toInt + 1)
      newState.run("2") shouldBe("2", 3)
    }
  }
