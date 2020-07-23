package lox

case class ParseState[A, State, Err](run: State => Either[Err, (State, A)]) {
  def map[B](f: A => B): ParseState[B, State, Err] = flatMap(a => ParseState(s => Right((s, f(a)))))
  def flatMap[B](f: A => ParseState[B, State, Err]): ParseState[B, State, Err] =
    ParseState(run.andThen(_.flatMap {
      case (s, a) => f(a).run(s)
    }))
}

object ParseState {
  def modify[S, Err](f: S => S): ParseState[Unit, S, Err]  = ParseState[Unit, S, Err](s => Right(f(s), ()))
  def inspect[S, B, Err](f: S => B): ParseState[B, S, Err] = ParseState[B, S, Err](s => Right(s, f(s)))
  def get[S, Err]: ParseState[S, S, Err]                   = inspect(identity)
  def pure[S, A, Err](a: A): ParseState[A, S, Err]         = ParseState(s => Right(s, a))

  def fail[S, A, Err](reason: Err): ParseState[A, S, Err] = ParseState(_ => Left(reason))
}

