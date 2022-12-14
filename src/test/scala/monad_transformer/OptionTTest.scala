package monad_transformer

import domain.State
import functor.Functor
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class OptionTTest extends AnyWordSpecLike with Matchers:

  import OptionT.given
  import functor.Functor.given
  import functor.StateFunctorInstance.given
  import monad.Monad.given
  import monad.StateMonadInstance.given

  type StringState[X] = State[String, X]
  type App[X] = OptionT[StringState, X]

  "OptionT" should {
    "work for all present" in {
      val res: App[Int] = for {
        i <- 10.pure[App]
        j <- 20.pure[App]
      } yield i + j
      res.runOption.run("")._2 shouldBe Some(30)
    }
  }

  "OptionT" should {
    "work for absent" in {
      val res: App[Int] = for {
        i <- 10.pure[App]
        j <- 20.pure[App]
        k <- OptionT.empty
      } yield i + j
      res.runOption.run("")._2 shouldBe None
    }
  }
