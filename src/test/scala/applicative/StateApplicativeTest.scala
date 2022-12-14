package applicative

import domain.State
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class StateApplicativeTest extends AnyWordSpecLike with Matchers:

  import Applicative.given
  import StateApplicativeInstance.given
  import functor.Functor.given
  import functor.StateFunctorInstance.given

  "state applicative" should {
    "map value" in {
      val fx = (a: Int, b: Int, c: Int) => a + b + c
      val res: State[String, Int] = fx.curried `<$>` 2.pure[String] <*> 3.pure[String] <*> 4.pure[String]
      res.run("state") shouldBe("state", 9)
    }
  }
