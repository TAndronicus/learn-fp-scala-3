package applicative

import domain.State

object StateApplicativeInstance:
  given applicativeInstance[S]: Applicative[[A] =>> State[S, A]] with
    override def pure[A](a: A): State[S, A] = State(s => (s, a))

    override def <*>[A, B](ffx: State[S, A => B])(fa: State[S, A]): State[S, B] = State(s => {
      val (fas, faa) = fa.run(s)
      val (ffxs, fx) = ffx.run(fas)
      (ffxs, fx(faa))
    })

  class StatePureInstance[A](a: A):
    def pure[S]: State[S, A] = applicativeInstance[S].pure(a)

  given toStatePureInstance[A]: Conversion[A, StatePureInstance[A]] with
    override def apply(x: A): StatePureInstance[A] = new StatePureInstance(x)
