package monad

trait Monad[F[_]]:
  def flatMap[A, B](fa: F[A])(fx: A => F[B]): F[B]

  def pure[A](a: A): F[A]

case class MonadFxOps[A, F[_]](fa: F[A])(implicit monad: Monad[F]):
  def flatMap[B](fx: A => F[B]): F[B] = monad.flatMap(fa)(fx)

case class MonadOps[A](a: A):
  def pure[F[_]](using monad: Monad[F]): F[A] = monad.pure(a)

object Monad:
  given toMonadFxOps[A, F[_] : Monad]: Conversion[F[A], MonadFxOps[A, F]] with
    override def apply(x: F[A]): MonadFxOps[A, F] = MonadFxOps(x)

  given toMonadOps[A, F[_] : Monad]: Conversion[A, MonadOps[A]] with
    override def apply(a: A): MonadOps[A] = MonadOps(a)
