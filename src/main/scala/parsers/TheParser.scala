package parsers

import scala.util.matching.Regex

type Parser[A] = Location => Result[A]

enum Result[A]:
  case Success(get: A, charsConsumed: Int)
  case Failure(get: ParseError)
  
  def mapError(f: ParseError => ParseError): Result[A] = this match 
    case Failure(e) => Failure(f(e))
    case _ => this
  
  def recoverWith(f: ParseError => Result[A]): Result[A] = this match 
    case Failure(e) => f(e)
    case s => s

case class ParseError(stack: List[(Location, String)]):
  def push(loc: Location, msg: String): ParseError = copy(stack = (loc, msg) :: stack)
  def label[A](s: String): ParseError = ParseError(latest.map((_, s)).toList)
  def latest: Option[Location] = stack.lastOption.map(_._1)

case class Location(input: String, offset: Int = 0):
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val next = input.drop(offset)

trait Parsers:
  def pure[A](a: A): Parser[A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def string(c: String): Parser[String]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] = (1 to n).foldLeft(pure(List.empty[A]))((parser,_) => map2(p, parser)(_ :: _))
  def count(c: Char): Parser[Int] = char(c).many.slice.map(_.length)
  def until(c: Char): Parser[String]

  def [A] (s1: Parser[A]) | (s2: => Parser[A]): Parser[A]
  def [A, B] (s1: Parser[A]) map (f: A => B): Parser[B] = s1.flatMap(a => pure(f(a)))
  def [A, B] (s1: Parser[A]) flatMap (f: A => Parser[B]): Parser[B]
  def [A, B] (s1: Parser[A]) *> (s2: Parser[B]): Parser[B] = map2(s1, s2)((_, b) => b)
  def [A, B] (s1: Parser[A]) <* (s2: Parser[B]): Parser[A] = map2(s1, s2)((a, _) => a)
  def [A, B] (s1: Parser[A]) ** (s2: => Parser[B]): Parser[(A, B)] = s1.flatMap(a => s2.map(b => (a,b)))
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] = (p ** p2).map(f.tupled)

  def [A](p: Parser[A]) slice: Parser[String]
  def [A](p: Parser[A]) many: Parser[List[A]] = map2(p, p.many)(_ :: _) | pure(List.empty)
  def [A](p: Parser[A]) many1: Parser[List[A]] = map2(p , p.many)((a, as) => a :: as).scope("Expected one or more of: ")
  def [A](p: Parser[A]) ?(default: A): Parser[A] = p | pure(default)

  def (s: String) asParser: Parser[String] = string(s)
  def (r: Regex) asParser: Parser[String] 
  
  def [A, B](p: Parser[A]) line (f: (Int, A) => B): Parser[B]
  //errors
  def [A](p: Parser[A]) label (msg: String): Parser[A]
  def [A](p: Parser[A]) scope (msg: String): Parser[A]
  
  def digit: Parser[Double] = """[0-9]+\.?[0-9]*""".r.asParser.map(_.toDouble)
  
  def alphaChar: Parser[Char] = "[a-zA-z]".r.asParser.map(_.head)
  def alphaNumChar: Parser[Char] = alphaChar | "[0-9]".r.asParser.map(_.head)
  
  def alphaNum: Parser[String] = alphaNumChar.many1.map(_.mkString)
  
  def identifier: Parser[String] = map2(alphaChar, alphaNum ? "")((c, rest) => s"$c$rest")

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def countX(c: Char): Parser[String] = "[0-9]".r.asParser.map(_.toInt).flatMap(needed => listOfN(needed, char(c))).map(_.mkString)

object TheParser extends Parsers:
  def pure[A](a: A): Parser[A] = l => Result.Success(a, l.offset)

  def until(c: Char): Parser[String] = firstLoc =>
    def rec(c: Char, startedAt: Int): Parser[String] = l=>
      if (l.next.startsWith(c.toString)) Result.Success(l.input.slice(startedAt, l.offset), l.offset) 
      else if (l.next.isEmpty) Result.Failure(ParseError(List((l, s"Reached end and did not find $c")))) // Fail
      else rec(c, startedAt)(l.copy(offset = l.offset + 1))
    rec(c, firstLoc.offset)(firstLoc)

  def string(c: String): Parser[String] = location =>
    if (location.next.startsWith(c)) Result.Success(c, location.offset + c.length)
    else Result.Failure(ParseError(List((Location(location.input), s"Expected: $c"))))
       
  def (r: Regex) asParser: Parser[String] = 
    val msg = "regex " + r
    s => r.findPrefixOf(s.next) match 
      case None => Result.Failure(ParseError(List((Location(s.input), msg))))
      case Some(m) => Result.Success(m,s.offset + m.length)
  
  def [A](p: Parser[A]) slice: Parser[String]  = l => 
    p(l) match 
      case Result.Success(res, newOffset) => Result.Success(l.input.take(newOffset), newOffset)
      case f => f.asInstanceOf[Result[String]]

  def [A, B] (s1: Parser[A]) flatMap (f: A => Parser[B]): Parser[B] = l =>
    s1(l) match 
      case Result.Success(a, consumed) => f(a)(Location(l.input, consumed)) 
      case fail => fail.asInstanceOf[Result[B]]
  
  def [A] (s1: Parser[A]) | (s2: => Parser[A]): Parser[A] = l => s1(l).recoverWith(_ => s2(l))

  def [A, B](p: Parser[A]) line (f: (Int, A) => B): Parser[B] = s => p.map(a => f(s.line, a))(s)
  
  def [A](p: Parser[A]) label (msg: String): Parser[A] = s => p(s).mapError(_.label(msg))
  def [A](p: Parser[A]) scope (msg: String): Parser[A] = s => p(s).mapError(_.push(s, msg))
  

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] = p(Location(input)) match 
    case Result.Success(a, _) => Right(a)
    case Result.Failure(err) => Left(err)


def runSpec(p: Parsers): Unit =
  import p._
  assert(run(char('c'))('c'.toString) == Right('c'))
  assert(run("c".asParser)("c") == Right("c"))
  assert(run("abra".asParser | "cadabra".asParser)("abra") == Right("abra"))
  assert(run("abra".asParser | "cadabra".asParser)("cadabra") == Right("cadabra"))
  assert(run(listOfN(3, "ab".asParser | "cad".asParser))("ababcad") == Right(List("ab","ab", "cad")))
  assert(run(count('a'))("aaabcad") == Right(3))
  assert(run(char('a').many1.map(_.length))("ababcad") == Right(1))
  assert(run(char('a').many1)("bbbbbbb") 
    == Left(ParseError(List((Location("bbbbbbb", 0), "Expected one or more of: "), (Location("bbbbbbb", 0), "Expected: a")))))
  assert(run(digit)("234.04") == Right(234.04))
  assert(run(alphaChar)("c") == Right('c'))
  assert(run(alphaNumChar)("8") == Right('8'))
  assert(run(alphaNum)("8asd34SDQ") == Right("8asd34SDQ"))
  assert(run(identifier)("asd34SDQ") == Right("asd34SDQ"))
  assert(run(identifier)("a") == Right("a"))
  assert(run(identifier)("a8") == Right("a8"))
  assert(run(until('c'))("dascasc") == Right("das"))


