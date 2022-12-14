package monad

import domain.State

object StateMonadInstance:
  given stateMonad[S]: Monad[[X] =>> State[S, X]] with
    override def flatMap[A, B](fa: State[S, A])(fx: A => State[S, B]): State[S, B] = State(s => {
      val (sa, a) = fa.run(s)
      fx(a).run(sa)
    })

    override def pure[A](a: A): State[S, A] = State(s => (s, a))
    
      
