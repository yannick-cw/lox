package lox

import scala.io.Source
import scala.io.StdIn
import lox.Errors.LoxError

object Lox:
  def runPropmt(): Unit = 
    println(run(StdIn.readLine("> ")).fold(Errors.error, x => x))
    runPropmt()
  
  def run(content: String): Either[LoxError, Any] =
    scanTokens(content).map(_ :+ EOF("", null, 0))
      .flatMap(Parser.parse)
      .map { a =>
        println(Parser.printExpr(a)); a
      }
      .flatMap(Interpreter.interpret)

  def runFile(filePath: String) = run(Source.fromFile(filePath).getLines.mkString)

  def main(args: Array[String]): Unit =     
    args.toList match 
      case Nil => runPropmt()
      case x :: Nil =>
        runFile(x).left.foreach { err =>
          Errors.error(err)
          System.exit(65)
        }
      case _ =>
        println("Usage: slox [script file]")
        System.exit(64)
    
  

object Errors:
  case class LoxError(line: Int, msg: String)
  def error(loxError: LoxError) = 
    val where = ""
    println(s"[line ${loxError.line} ] Error $where : ${loxError.msg}]")
