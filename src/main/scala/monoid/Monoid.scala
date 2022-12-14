package monoid

trait Monoid[A]:
  def mzero: A

  def mappend(left: A, right: A): A

object Monoid:
  def mzero[A](using m: Monoid[A]) = m.mzero

  given monoidOpsConversion[A: Monoid]: Conversion[A, MonoidOps[A]] with
    override def apply(x: A): MonoidOps[A] = MonoidOps(x)

class MonoidOps[A](left: A)(using m: Monoid[A]):
  def |+|(right: A) = m.mappend(left, right)


