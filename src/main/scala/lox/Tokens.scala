package lox

enum Expr:
  case Binary(l: Expr, op: Operator, r: Expr)
  case Unary(op: Operator, e: Expr)
  case Grouping(e: Expr)
  case LiteralE(l: lox.Literal)
  def fold[B](fb: Binary => B, fu: Unary => B, fg: Grouping => B, fl: LiteralE => B): B = this match
    case b: Binary => fb(b)
    case b: Unary => fu(b)
    case b: Grouping => fg(b)
    case b: LiteralE => fl(b)

sealed trait Operator:
  def line: Int
  def fold[B](
               fBang: Bang => B,
               fEqEq: EqEq => B,
               fNotEq: BangEq => B,
               fSmaller: Less => B,
               fSmallerEq: LessEq => B,
               fGreater: Greater => B,
               fGreaterEq: GreaterEq => B,
               fPlus: Plus => B,
               fMinusOp: Minus => B,
               fStar: Star => B,
               fDivide: Slash => B
             ) = this match
    case n: EqEq => fEqEq(n)
    case n: BangEq => fNotEq(n)
    case n: Bang => fBang(n)
    case n: Less => fSmaller(n)
    case n: LessEq => fSmallerEq(n)
    case n: Greater => fGreater(n)
    case n: GreaterEq => fGreaterEq(n)
    case n: Plus => fPlus(n)
    case n: Minus => fMinusOp(n)
    case n: Star => fStar(n)
    case n: Slash => fDivide(n)
      
case class Bang(lexeme: String, literal: Any, line: Int) extends Operator
case class Minus(lexeme: String, literal: Any, line: Int) extends Operator
case class Plus(lexeme: String, literal: Any, line: Int) extends Operator
case class BangEq(lexeme: String, literal: Any, line: Int) extends Operator
case class EqEq(lexeme: String, literal: Any, line: Int) extends Operator
case class Greater(lexeme: String, literal: Any, line: Int) extends Operator
case class GreaterEq(lexeme: String, literal: Any, line: Int) extends Operator
case class Less(lexeme: String, literal: Any, line: Int) extends Operator
case class LessEq(lexeme: String, literal: Any, line: Int) extends Operator
case class Star(lexeme: String, literal: Any, line: Int) extends Operator
case class Slash(lexeme: String, literal: Any, line: Int) extends Operator
  

object Operator:
  def  bang(lexeme: String, literal: Any, line: Int): Token = OperatorW(Bang(lexeme, literal, line))
  def  minus(lexeme: String, literal: Any, line: Int): Token = OperatorW(Minus(lexeme, literal, line))
  def  plus(lexeme: String, literal: Any, line: Int): Token = OperatorW(Plus(lexeme, literal, line))
  def  bangEq(lexeme: String, literal: Any, line: Int): Token = OperatorW(BangEq(lexeme, literal, line))
  def  eqEq(lexeme: String, literal: Any, line: Int): Token = OperatorW(EqEq(lexeme, literal, line))
  def  greater(lexeme: String, literal: Any, line: Int): Token = OperatorW(Greater(lexeme, literal, line))
  def  greaterEq(lexeme: String, literal: Any, line: Int): Token = OperatorW(GreaterEq(lexeme, literal, line))
  def  less(lexeme: String, literal: Any, line: Int): Token = OperatorW(Less(lexeme, literal, line))
  def  lessEq(lexeme: String, literal: Any, line: Int): Token = OperatorW(LessEq(lexeme, literal, line))
  def  star(lexeme: String, literal: Any, line: Int): Token = OperatorW(Star(lexeme, literal, line))
  def  slash(lexeme: String, literal: Any, line: Int): Token = OperatorW(Slash(lexeme, literal, line))

sealed trait Literal:
  def line: Int
  def literal: Any
  def fold[B](
               fNumber: Number => B,
               fString: StringToken => B,
               fTrue: True => B,
               fNil: NilT => B,
               fFalse: False => B
             ): B =
    this match
      case n: Number => fNumber(n)
      case n: StringToken => fString(n)
      case n: True => fTrue(n)
      case n: False => fFalse(n)
      case n: NilT => fNil(n)

case class Number(lexeme: String, literal: Any, line: Int) extends Literal
case class StringToken(lexeme: String, literal: Any, line: Int) extends Literal
case class True(lexeme: String, literal: Any, line: Int) extends Literal
case class False(lexeme: String, literal: Any, line: Int)extends Literal
case class NilT(lexeme: String, literal: Any, line: Int)extends Literal
    

object Literal:
  def number(lexeme: String, literal: Any, line: Int): Token = LiteralW(Number(lexeme, literal, line))
  def stringToken(lexeme: String, literal: Any, line: Int): Token = LiteralW(StringToken(lexeme, literal, line))
  def trueC(lexeme: String, literal: Any, line: Int): Token = LiteralW(True(lexeme, literal, line))
  def falseC(lexeme: String, literal: Any, line: Int): Token = LiteralW(False(lexeme, literal, line))
  def nilT(lexeme: String, literal: Any, line: Int): Token = LiteralW(NilT(lexeme, literal, line))


sealed trait Token:
  def line: Int

case class LiteralW(l: lox.Literal) extends Token:
  val line: Int = l.line
case class OperatorW(op: lox.Operator) extends Token:
  val line: Int = op.line
case class LeftParen(lexeme: String, literal: Any, line: Int)  extends Token
case class RightParen(lexeme: String, literal: Any, line: Int)  extends Token
case class LeftBrace(lexeme: String, literal: Any, line: Int)  extends Token
case class RightBrace(lexeme: String, literal: Any, line: Int)  extends Token
case class Comma(lexeme: String, literal: Any, line: Int)  extends Token
case class Dot(lexeme: String, literal: Any, line: Int)  extends Token
case class Semicolin(lexeme: String, literal: Any, line: Int)  extends Token
case class Identifier(lexeme: String, literal: Any, line: Int)  extends Token
case class And(lexeme: String, literal: Any, line: Int)  extends Token
case class Class(lexeme: String, literal: Any, line: Int)  extends Token
case class Else(lexeme: String, literal: Any, line: Int)  extends Token
case class Fun(lexeme: String, literal: Any, line: Int)  extends Token
case class For(lexeme: String, literal: Any, line: Int)  extends Token
case class If(lexeme: String, literal: Any, line: Int)  extends Token
case class Or(lexeme: String, literal: Any, line: Int)  extends Token
case class Print(lexeme: String, literal: Any, line: Int)  extends Token
case class Return(lexeme: String, literal: Any, line: Int)  extends Token
case class Super(lexeme: String, literal: Any, line: Int)  extends Token
case class This(lexeme: String, literal: Any, line: Int)  extends Token
case class Var(lexeme: String, literal: Any, line: Int)  extends Token
case class While(lexeme: String, literal: Any, line: Int)  extends Token
case class Eq(lexeme: String, literal: Any, line: Int) extends Token
case class EOF(lexeme: String, literal: Any, line: Int)  extends Token
