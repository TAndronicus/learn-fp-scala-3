package monoid

import domain.PairAdditive
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class PairAdditiveMonoidTest extends AnyWordSpecLike with Matchers:

  import ListMonoid.given
  import Monoid.given
  import PairAdditiveMonoid.given
  import SimpleMonoid.given

  "pair monoid" should {
    "operate on nested structures" in {
      PairAdditive(PairAdditive(Sum(1), Product(2)), PairAdditive(List(3, 33, 333), Sum(4))) |+|
        PairAdditive(PairAdditive(Sum(21), Product(22)), PairAdditive(List(23, 233, 2333), Sum(24))) shouldBe
        PairAdditive(PairAdditive(Sum(1 + 21), Product(2 * 22)), PairAdditive(List(3, 33, 333, 23, 233, 2333), Sum(4 + 24)))
    }
    "obey identity" in {
      PairAdditive(Sum(10), Product(20)) |+| Monoid.mzero[PairAdditive[Sum, Product]] shouldBe PairAdditive(Sum(10), Product(20))
      PairAdditive(List(1, 2, 3), Sum(1)) |+| Monoid.mzero[PairAdditive[List[Int], Sum]] shouldBe PairAdditive(List(1, 2, 3), Sum(1))
      PairAdditive(PairAdditive(Sum(1), Sum(2)), PairAdditive(Sum(3), Sum(4))) |+|
        Monoid.mzero[PairAdditive[PairAdditive[Sum, Sum], PairAdditive[Sum, Sum]]] shouldBe PairAdditive(PairAdditive(Sum(1), Sum(2)), PairAdditive(Sum(3), Sum(4)))
    }

    "obey associtiativity" in {
      val a = PairAdditive(PairAdditive(Sum(1), Product(2)), PairAdditive(List(3, 33, 333), Sum(4)))
      val b = PairAdditive(PairAdditive(Sum(21), Product(22)), PairAdditive(List(23, 233, 2333), Sum(24)))
      val c = PairAdditive(PairAdditive(Sum(31), Product(32)), PairAdditive(List(33, 333, 3333), Sum(34)))
      a |+| b |+| c shouldBe a |+| (b |+| c)
    }
  }
