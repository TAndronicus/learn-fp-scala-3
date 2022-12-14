package functor

import domain.State

object StateFunctorInstance:
  //  given stateFunctor[S]: Functor[({type E[X] = State[S, X]})#E] with
  //    override def map[A, B](a: State[S, A])(fx: A => B): State[S, B] = State(a.run.andThen { case (s, a) => (s, fx(a)) })

  //  type StateSA = [S] =>> [A] =>> State[S, A]

  //  given stateFunctor[S]: Functor[StateSA[S]] with
  //    override def map[A, B](a: State[S, A])(fx: A => B): State[S, B] = State(a.run.andThen { case (s, a) => (s, fx(a)) })

  given stateFunctor[S]: Functor[[A] =>> State[S, A]] with
    override def map[A, B](a: State[S, A])(fx: A => B): State[S, B] = State(a.run.andThen { case (s, a) => (s, fx(a)) })

//  given stateFunctor[S]: Functor[State[S, ?]] with
//    override def map[A, B](a: State[S, A])(fx: A => B): State[S, B] = State(a.run.andThen { case (s, a) => (s, fx(a)) })
