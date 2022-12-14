package monad

import domain.State
import functor.Functor
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class StateMonadTest extends AnyWordSpecLike with Matchers:

  import Monad.given
  import StateMonadInstance.stateMonad
  import applicative.StateApplicativeInstance.given
  import functor.Functor.given
  import functor.StateFunctorInstance.given

  "state monad" should {
    "work in for" in {
      def addOne(a: Int): State[String, Int] = State(s => (s, a + 1))

      val res: State[String, Int] = for {
        a <- 1.pure[String]
        b <- addOne(a)
      } yield b
      res.run("a") shouldBe("a", 2)
    }
  }
