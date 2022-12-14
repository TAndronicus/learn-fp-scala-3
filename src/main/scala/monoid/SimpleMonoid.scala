package monoid

case class Sum(i: Int)

case class Product(i: Int)

object SimpleMonoid:
  given Monoid[Sum] with
    override def mzero: Sum = Sum(0)

    override def mappend(left: Sum, right: Sum): Sum = Sum(left.i + right.i)

  given Monoid[Product] with
    override def mzero: Product = Product(1)

    override def mappend(left: Product, right: Product): Product = Product(left.i * right.i)
