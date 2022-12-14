package monad_transformer

import functor.Functor
import monad.Monad

case class OptionT[F[_], A](runOption: F[Option[A]])

object OptionT:

  import Functor.given
  import Monad.given

  def lift[F[_], A](fa: F[A])(using Functor[F]): OptionT[F, A] = OptionT(fa.map(Option(_)))

  def empty[F[_], A](using monad: Monad[F]): OptionT[F, A] = OptionT(monad.pure(None))

  given optionTFunctorInstance[F[_]] (using Functor[F]): Functor[[X] =>> OptionT[F, X]] with
    override def map[A, B](ot: OptionT[F, A])(fx: A => B): OptionT[F, B] = OptionT(ot.runOption.map(_.map(fx)))

  given optionTMonadInstance[F[_]] (using f: Functor[F], m: Monad[F]): Monad[[X] =>> OptionT[F, X]] with
    override def flatMap[A, B](ot: OptionT[F, A])(fx: A => OptionT[F, B]): OptionT[F, B] = OptionT(ot.runOption.flatMap {
      case Some(a) => fx(a).runOption
      case None => m.pure[Option[B]](None)
    })

    override def pure[A](a: A): OptionT[F, A] = OptionT(m.pure(Some(a)))
