package domain

case class State[S, A](run: S => (S, A)):
  def exec(s: S): S = run(s)._1

  def eval(s: S): A = run(s)._2


object State:
  def put[S](ns: S): State[S, Unit] = State(_ => (ns, ()))

  def get[S]: State[S, S] = State(s => (s, s))

  def modify[S](fx: S => S): State[S, Unit] = State(s => (fx(s), ()))
