package applicative

trait Applicative[F[_]]:
  def pure[A](a: A): F[A]

  def <*>[A, B](ffx: F[A => B])(fa: F[A]): F[B]

class FxApplicativeOps[A, B, F[_]](ffx: F[A => B])(implicit applicative: Applicative[F]):
  def <*>(fa: F[A]) = applicative.<*>(ffx)(fa)

object Applicative:
  given toFxApplicativeOps[A, B, F[_] : Applicative]: Conversion[F[A => B], FxApplicativeOps[A, B, F]] with
    override def apply(ffx: F[A => B]) = new FxApplicativeOps(ffx)

