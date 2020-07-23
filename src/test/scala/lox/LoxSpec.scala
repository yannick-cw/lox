package lox

import org.junit.Test
import org.junit.Assert._

class LoxSpec:
  @Test def simpleExpression(): Unit =
    val source = """(99.0)""".stripMargin
    val res = scanTokens(source)
    assertEquals(Right(List(LeftParen("(",null,1), LiteralW(Number("99.0",99.0,1)), RightParen(")",null,1))), res)

  @Test def allowsSpecialNameInIdentifier(): Unit = {
    val source = """classIsNoClass""".stripMargin
    val res = scanTokens(source)
    assertEquals(Right(List(Identifier("classIsNoClass", null, 1))), res)
  }

  @Test def classFile(): Unit =
    val source =
      """
        |class A {
        |  var name09 = 99.01
        |}
        |""".stripMargin
    assertEquals(Right((List(Class("class",null,2), Identifier("A",null,2), LeftBrace("{",null,2), Var("var",null,3), Identifier("name09",null,3),
      Eq("=",null,3), LiteralW(Number("99.01",99.01,3)), RightBrace("}",null,4)))), scanTokens(source))

  @Test def fileWithComment(): Unit =
    val source =
      """
        |// Test Test  
        |22.0
        |""".stripMargin
    assertEquals(Right((List(LiteralW(Number("22.0", 22.0, 3))))), scanTokens(source))
    
  @Test def commentInLine(): Unit =
    val source =
      """
        |22.0 // Test Test  
        |""".stripMargin
    assertEquals(Right((List(LiteralW(Number("22.0", 22.0, 2))))), scanTokens(source))

  @Test def complexClassFile(): Unit =
    val source =
      """
        |class A {
        |  var name09 = 99.01;
        |  var x = (22/3 ) * 124 + 12;
        |  var z = false == true
        |  var uiii = false != true
        |  var compare = 22 <= 3 < 4 >5 >= 12
        |  fun xx(name) = name and "tru"
        |}
        |""".stripMargin
    assertEquals(Right(
      List(Class("class",null,2),
        Identifier("A",null,2),
        LeftBrace("{",null,2),
        Var("var",null,3),
        Identifier("name09",null,3),
        Eq("=",null,3),
        LiteralW(Number("99.01",99.01,3)),
        Semicolin(";",null,3),
        Var("var",null,4),
        Identifier("x",null,4),
        Eq("=",null,4),
        LeftParen("(",null,4),
        LiteralW(Number("22.0",22.0,4)),
        OperatorW(Slash("/",null,4)),
        LiteralW(Number("3.0",3.0,4)),
        RightParen(")",null,4),
        OperatorW(Star("*",null,4)),
        LiteralW(Number("124.0",124.0,4)),
        OperatorW(Plus("+",null,4)),
        LiteralW(Number("12.0",12.0,4)),
        Semicolin(";",null,4),
        Var("var",null,5),
        Identifier("z",null,5),
        Eq("=",null,5),
        LiteralW(False("false",null,5)),
        OperatorW(EqEq("==",null,5)),
        LiteralW(True("true",null,5)),
        Var("var",null,6),
        Identifier("uiii",null,6),
        Eq("=",null,6),
        LiteralW(False("false",null,6)),
        OperatorW(Bang("!",null,6)),
        Eq("=",null,6),
        LiteralW(True("true",null,6)),
        Var("var",null,7),
        Identifier("compare",null,7),
        Eq("=",null,7),
        LiteralW(Number("22.0",22.0,7)),
        OperatorW(LessEq("<=",null,7)),
        LiteralW(Number("3.0",3.0,7)),
        OperatorW(Less("<",null,7)),
        LiteralW(Number("4.0",4.0,7)),
        OperatorW(Greater(">",null,7)),
        LiteralW(Number("5.0",5.0,7)),
        OperatorW(GreaterEq(">=",null,7)),
        LiteralW(Number("12.0",12.0,7)),
        Fun("fun",null,8),
        Identifier("xx",null,8),
        LeftParen("(",null,8),
        Identifier("name",null,8),
        RightParen(")",null,8),
        Eq("=",null,8),
        Identifier("name",null,8),
        And("and",null,8),
        LiteralW(StringToken("tru", "tru", 8)),
        RightBrace("}",null,9))),
      scanTokens(source))
      
