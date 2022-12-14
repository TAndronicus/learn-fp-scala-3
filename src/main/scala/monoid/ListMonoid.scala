package monoid

object ListMonoid:
  given listMonoid[T]: Monoid[List[T]] with
    override def mzero: List[T] = List()

    override def mappend(left: List[T], right: List[T]): List[T] = left ::: right

