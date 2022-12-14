package monoid

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class SimpleMonoidTest extends AnyWordSpecLike with Matchers:

  import Monoid.given
  import SimpleMonoid.given

  "sum monoid" should {
    "sum numbers" in {
      Sum(10) |+| Sum(20) shouldBe Sum(30)
    }
    "obey identity" in {
      Sum(10) |+| Monoid.mzero[Sum] shouldBe Sum(10)
      Monoid.mzero[Sum] |+| Sum(10) shouldBe Sum(10)
    }
    "obey associativity" in {
      Sum(10) |+| Sum(20) |+| Sum(30) shouldBe Sum(10) |+| (Sum(20) |+| Sum(30))
    }
  }

  "product monoid" should {
    "multiply numbers" in {
      Product(10) |+| Product(20) shouldBe Product(200)
    }
    "obey identity" in {
      Product(10) |+| Monoid.mzero[Product] shouldBe Product(10)
      Monoid.mzero[Product] |+| Product(10) shouldBe Product(10)
    }
    "obey associativity" in {
      Product(10) |+| Product(20) |+| Product(30) shouldBe Product(10) |+| (Product(20) |+| Product(30))
    }
  }
