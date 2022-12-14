package monoid

import domain.PairAdditive

object PairAdditiveMonoid:

  import Monoid.given

  given pairAdditiveMonoid[A, B] (using am: Monoid[A], bm: Monoid[B]): Monoid[PairAdditive[A, B]] with
    override def mzero: PairAdditive[A, B] = PairAdditive(am.mzero, bm.mzero)

    override def mappend(left: PairAdditive[A, B], right: PairAdditive[A, B]): PairAdditive[A, B] = PairAdditive(left.left |+| right.left, left.right |+| right.right)
