package monad

object OptionMonadInstance:
  given optionMonad: Monad[Option] with
    override def pure[A](a: A): Option[A] = Some(a)

    override def flatMap[A, B](fa: Option[A])(fx: A => Option[B]): Option[B] = fa match
      case Some(value) => fx(value)
      case None => None
