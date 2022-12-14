package functor

trait Functor[F[_]]:
  def map[A, B](a: F[A])(fx: A => B): F[B]

case class FunctorOps[A, F[_]](a: F[A])(implicit functor: Functor[F]):
  def map[B](fx: A => B): F[B] = functor.map(a)(fx)

case class FxFunctorOps[A, B, F[_]](fx: A => B)(implicit functor: Functor[F]):
  def `<$>`(a: F[A]): F[B] = functor.map(a)(fx)

object Functor:
  given toFunctorOps[A, F[_] : Functor]: Conversion[F[A], FunctorOps[A, F]] with
    override def apply(a: F[A]): FunctorOps[A, F] = FunctorOps(a)

  given toFxFunctorOps[A, B, F[_] : Functor]: Conversion[A => B, FxFunctorOps[A, B, F]] with
    override def apply(fx: A => B): FxFunctorOps[A, B, F] = FxFunctorOps(fx)
