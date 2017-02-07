package de.hyronx.matter.compiler

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object HelloWorld extends App {
  if (args.length == 0)
    println("Please provide some arguments!")

  for (arg ← args) {
    try {
      val code = Source.fromFile(arg).mkString
      Lexer(code) match {
        case Left(LexerError(msg)) ⇒ println("Lexer failed: " + msg)
        case Right(result) ⇒
          println(s"Tokens: ${result}")
          println(Parser(result))
      }
    } catch {
      case e: java.io.FileNotFoundException ⇒ println(e.getMessage)
    }
  }
}
