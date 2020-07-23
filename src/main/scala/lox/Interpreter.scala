package lox

import lox.Errors.LoxError
import lox.Expr._
import scala.reflect.ClassTag

object Interpreter {

  def interpret(expr: Expr): Either[LoxError, Any] =
    expr.fold(
      fu = interpretUnary,
      fb = interpretBinary,
      fg = g => interpret(g.e),
      fl = l => Right(interpretLit(l))
    )

  private def interpretLit(lit: LiteralE): Any = lit.l.literal
  private def interpretUnary(unary: Unary): Either[LoxError, Any] =
    for {
      right <- interpret(unary.e)
      res <- unary.op.fold(
        fBang => isTruthy(right).map(!_),
        fEqEq = ???,
        fMinusOp => safeCast[Double](right, unary.op.line).map(-_),
        fDivide = ???,
        fStar = ???,
        fPlus = ???,
        fGreater = ???,
        fGreaterEq = ???,
        fSmaller = ???,
        fSmallerEq = ???,
        fNotEq = ???
      )
    } yield res

  private def combineBinary[A: ClassTag](
      left: Any,
      right: Any,
      line: Int
  ): ((A, A) => Any) => Either[LoxError, Any] =
    op =>
      for {
        l <- safeCast[A](left, line)
        r <- safeCast[A](right, line)
      } yield op(l, r)

  private def interpretBinary(expr: Binary): Either[LoxError, Any] =
    for {
      left  <- interpret(expr.l)
      right <- interpret(expr.r)
      combineDouble = combineBinary[Double](left, right, expr.op.line)
      combineString = combineBinary[String](left, right, expr.op.line)
      res <- expr.op.fold(
        fBang = ele => throw new RuntimeException(s"what are you doing here? $ele"),
        fMinusOp = _ => combineDouble((a, b) => a - b),
        fDivide = _ => combineDouble((a, b) => a / b),
        fStar = _ => combineDouble((a, b) => a * b),
        fPlus = _ =>
          if (left.isInstanceOf[Double]) combineDouble((a, b) => a + b)
          else if (left.isInstanceOf[String]) combineString((a, b) => a + b)
          else Left(LoxError(expr.op.line, s"Can not + $left and $right")),
        fGreater = _ => combineDouble((a, b) => a > b),
        fGreaterEq = _ => combineDouble((a, b) => a >= b),
        fSmaller = _ => combineDouble((a, b) => a < b),
        fSmallerEq = _ => combineDouble((a, b) => a <= b),
        fEqEq = _ => Right(left == right),
        fNotEq = _ => Right(left != right)
      )

    } yield res

  private def isTruthy(a: Any): Either[LoxError, Boolean] =
    safeCast[Boolean](a, 0)

  private def safeCast[To: ClassTag](a: Any, line: Int): Either[LoxError, To] =
    a match {
      case a: To => Right(a.asInstanceOf[To])
      case other => Left(LoxError(line, s"$a is not of type"))
    }
}
