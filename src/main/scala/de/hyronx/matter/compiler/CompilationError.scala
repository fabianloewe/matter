package de.hyronx.matter.compiler

trait CompilationError
case class LexerError(msg: String) extends CompilationError
case class ParserError(msg: String) extends CompilationError
