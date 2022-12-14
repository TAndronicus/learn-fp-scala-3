package free_monad

import functor.{Functor, FunctorOps}
import monad.{Monad, MonadFxOps}

trait Free[F[_], A]

sealed case class Return[F[_], A](a: A) extends Free[F, A]

sealed case class Lift[F[_], A](a: F[A]) extends Free[F, A]

sealed case class FlatMap[F[_], A, B](fa: Free[F, A], fx: A => Free[F, B]) extends Free[F, B]

abstract class Natural[F[_], G[_]]:
  def transform[A](fa: F[A]): G[A]

object Free:
  given freeFunctorInstance[F[_]]: Functor[[X] =>> Free[F, X]] with
    override def map[A, B](a: Free[F, A])(fx: A => B): Free[F, B] = FlatMap(a, fx.andThen(Return(_)))

  given freeMonadInstance[F[_]]: Monad[[X] =>> Free[F, X]] with
    override def pure[A](a: A): Free[F, A] = Return(a)

    override def flatMap[A, B](fa: Free[F, A])(fx: A => Free[F, B]): Free[F, B] = FlatMap(fa, fx)

  given pure[F[_], A]: Conversion[A, Free[F, A]] with
    override def apply(x: A): Free[F, A] = freeMonadInstance[F].pure(x)

  given lift[F[_], A]: Conversion[F[A], Free[F, A]] with
    override def apply(x: F[A]): Free[F, A] = Lift(x)

  given toFunctorOps[A, F[_]]: Conversion[Free[F, A], FunctorOps[A, [X] =>> Free[F, X]]] with
    override def apply(x: Free[F, A]): FunctorOps[A, [X] =>> Free[F, X]] = FunctorOps(x)

  given toMonadFxOps[A, F[_]]: Conversion[Free[F, A], MonadFxOps[A, [X] =>> Free[F, X]]] with
    override def apply(x: Free[F, A]): MonadFxOps[A, [X] =>> Free[F, X]] = MonadFxOps(x)(freeMonadInstance[F])

  // chaining implicit conversions not permitted
  //  given fxToMonadFxOps[A, F[_]]: Conversion[F[A], MonadFxOps[A, [X] =>> Free[F, X]]] with
  //    override def apply(x: F[A]): MonadFxOps[A, [X] =>> Free[F, X]] = MonadFxOps(Lift(x))
  //
  //  given fxToFunctorOps[A, F[_]]: Conversion[F[A], FunctorOps[A, [X] =>> Free[F, X]]] with
  //    override def apply(x: F[A]): FunctorOps[A, [X] =>> Free[F, X]] = FunctorOps(Lift(x))

  def fold[F[_], G[_], A](free: Free[F, A], natural: Natural[F, G])(implicit monad: Monad[G]): G[A] = free match
    case FlatMap(free, fx) => free match
      case FlatMap(ifree, ifx) => fold(FlatMap(ifree, a => FlatMap(ifx(a), fx)), natural)
      case Lift(fa) => monad.flatMap(natural.transform(fa))(a => fold(fx(a), natural))
      case Return(a) => fold(fx(a), natural)
    case Lift(fa) => natural.transform(fa)
    case Return(a) => monad.pure(a)
