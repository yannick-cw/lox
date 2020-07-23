package lox

import lox.Errors._
import parsers.TheParser._
import Matchers._

case class Agg(tokenBuffer: List[Char], tokens: List[Token], line: Int = 0)

object Matchers:
  private def createMatcher(c: Char, tkn: (String, Any, Int) => Token) = char(c).line((line, c) => tkn(c.toString, null, line))
  private def createMatcher(c: String, tkn: (String, Any, Int) => Token) = string(c).line((line, c) => tkn(c, null, line))

  val whitespaces = char('\n') | char(' ') | char('\t') | char('\r')
  val commentLineIgnorer = (string("//") <* until('\n')) *> pure(' ')
  
  val tokenMatchers = (
      createMatcher('(', LeftParen) |
      createMatcher(')', RightParen) |
      createMatcher('{', LeftBrace) |
      createMatcher('}', RightBrace) |
      createMatcher( '.', Dot) |
      createMatcher( '-', Operator.minus) |
      createMatcher( '+', Operator.plus) |
      createMatcher( ';', Semicolin) |
      createMatcher( '*', Operator.star) |
      createMatcher( '!', Operator.bang) |
      createMatcher( "<=", Operator.lessEq) |
      createMatcher( '<', Operator.less) |
      createMatcher( ">=", Operator.greaterEq) |
      createMatcher( '>', Operator.greater) |
      createMatcher( '/', Operator.slash) |
      createMatcher(',', Comma) |
      createMatcher("==", Operator.eqEq) |
      createMatcher('=', Eq) |
      digit.line((line, c) => Literal.number(c.toString, c, line))
      )
  
  val classM = createMatcher("class", Class.apply) <* whitespaces
  val varM = createMatcher("var", Var.apply) <* whitespaces
  val and = createMatcher("and", And.apply) <* whitespaces
  val elseM = createMatcher("else", Else.apply) <* whitespaces
  val falseM = createMatcher("false", Literal.falseC) <* whitespaces
  val forM = createMatcher("for", For.apply) <* whitespaces
  val funM = createMatcher("fun", Fun.apply) <* whitespaces
  val ifM = createMatcher("if", If.apply) <* whitespaces
  val nilM = createMatcher("nil", Literal.nilT) <* whitespaces
  val orM = createMatcher("or", Or.apply) <* whitespaces
  val printM = createMatcher("print", Print.apply) <* whitespaces
  val returnM = createMatcher("return", Return.apply) <* whitespaces
  val superM = createMatcher("super", Super.apply) <* whitespaces
  val thisM = createMatcher("this", This.apply) <* whitespaces
  val trueM = createMatcher("true", Literal.trueC) <* whitespaces
  val whileM = createMatcher("while", While.apply) <* whitespaces
  val identifierM: parsers.Parser[Token] = identifier.line((line, ident ) => Identifier(ident, null, line))
  val identMatchers = (classM | varM | and | elseM | falseM | forM | funM | ifM | nilM | orM | printM | returnM | superM | thisM | trueM | whileM | identifierM)
  val stringM = for {
    _ <- char('"')
    res <- until('"').line((a,b) => (a,b))
    (line, string) = res
     _ <- char('"')
  } yield Literal.stringToken(string,string, line)
  
  val matcher = (tokenMatchers | identMatchers | stringM)

def scanTokens(source: String): Either[LoxError, List[Token]] = 
 run(((whitespaces | commentLineIgnorer).map(_ => List.empty) | matcher.many1 ).many1.map(_.flatten))(source).left.map(parseError =>
 LoxError(0, parseError.stack.mkString)
)

