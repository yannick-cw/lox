package lox

import lox.Expr._
import lox.Errors.LoxError
import lox.ParseState._

object Parser {
  def printExpr(expr: Expr): String =
    val foldOp: Operator => String = _.fold(
      fBang = op => "!",
      fEqEq = op => "==",
      fNotEq = op => "!=",
      fSmaller = op => "<",
      fSmallerEq = op => "<=",
      fGreater = op => ">",
      fGreaterEq = op => ">=",
      fPlus = op => "+",
      fMinusOp = op => "-",
      fStar = op => "*",
      fDivide = op => "/"
    )
    expr.fold(
      fu = u => foldOp(u.op) + printExpr(u.e),
      fb = bin => {
        printExpr(bin.l) + foldOp(bin.op)
        ++ printExpr (bin.r)
      },
      fg = g => "(" ++ printExpr(g.e) ++ ")",
      fl = _.l.fold(
        fNumber = num => num.lexeme,
        fString = s => s.lexeme,
        fTrue = _ => "TRUE",
        fFalse = _ => "FALSE",
        fNil = _ => "NIL"
      )
    )

  def parse(tokens: List[Token]): Either[LoxError, Expr] =
    equality.run(State(tokens.head, tokens.tail.headOption, tokens.tail.tail)).map(_._2)

  case class State(current: Token, next: Option[Token], rest: List[Token])

  private def rec(
      expr: Expr,
      pf: PartialFunction[Token, Operator]
  ): ParseState[Expr, State, LoxError] =
    get.flatMap(state =>
      pf.andThen(op =>
        for {
          _       <- advance
          right   <- comparison
          recExpr <- rec(Binary(expr, op, right), pf)
        } yield recExpr
      ).applyOrElse(state.current, (_: Token) => pure(expr))
    )

  private def repeatWhileMatching(
      pf: PartialFunction[Token, Operator],
      nextLevel: ParseState[Expr, State, LoxError]
  ): ParseState[Expr, State, LoxError] =
    nextLevel.flatMap(leftExpr => rec(leftExpr, pf))

  private def expression: ParseState[Expr, State, LoxError] = equality

  private def equality: ParseState[Expr, State, LoxError] =
    repeatWhileMatching(
      {
        case OperatorW(e: Bang)   => e
        case OperatorW(e: BangEq) => e
      },
      nextLevel = comparison
    )

  private def unary: ParseState[Expr, State, LoxError] =
    for {
      s <- get
      expr <- s.current match {
        case OperatorW(e: Bang)  => matchUnary(e)
        case OperatorW(e: Minus) => matchUnary(e)
        case other                        => primary
      }
    } yield expr

  private def matchUnary(e: Operator): ParseState[Expr, State, LoxError] =
    for {
      _     <- advance
      right <- unary
    } yield Unary(e, right)

  private def primary: ParseState[Expr, State, LoxError] =
    for {
      s <- get
      expr <- s.current match {
        case LiteralW(e: False)       => advance.map(_ => LiteralE(e))
        case LiteralW(e: True)        => advance.map(_ => LiteralE(e))
        case LiteralW(e: NilT)        => advance.map(_ => LiteralE(e))
        case LiteralW(e: Number)      => advance.map(_ => LiteralE(e))
        case LiteralW(e: StringToken) => advance.map(_ => LiteralE(e))
        case e: LeftParen                     => matchGrouping
        case o                                => fail[State, Expr, LoxError](LoxError(o.line, s"Do not understand $o"))
      }
    } yield expr

  private def matchGrouping: ParseState[Expr, State, LoxError] =
    for {
      _    <- advance
      expr <- expression
      newS <- get
      grouping <-
        if (newS.current.isInstanceOf[RightParen]) advance.map(_ => Grouping(expr))
        else
          fail[State, Grouping, LoxError](
            LoxError(newS.current.line, "Expect ')' after expresstion")
          )
    } yield grouping

  private def multiplication: ParseState[Expr, State, LoxError] =
    repeatWhileMatching(
      {
        case OperatorW(e: Slash) => e
        case OperatorW(e: Star)  => e
      },
      nextLevel = unary
    )

  private def addition: ParseState[Expr, State, LoxError] =
    repeatWhileMatching(
      {
        case OperatorW(e: Minus) => e
        case OperatorW(e: Plus)  => e
      },
      nextLevel = multiplication
    )

  private def comparison: ParseState[Expr, State, LoxError] =
    repeatWhileMatching(
      {
        case OperatorW(e: Greater)   => e
        case OperatorW(e: GreaterEq) => e
        case OperatorW(e: Less)      => e
        case OperatorW(e: LessEq)    => e
      },
      nextLevel = addition
    )

  private def advance: ParseState[Unit, State, LoxError] =
    isAtEnd.flatMap(end =>
      if (!end)
        modify(s =>
          State(s.next.get, s.rest.headOption, if (s.rest.nonEmpty) s.rest.tail else s.rest)
        )
      else pure(())
    )

  private def isAtEnd: ParseState[Boolean, State, LoxError] =
    inspect(s => s.current.isInstanceOf[EOF])
}
