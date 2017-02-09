package de.hyronx.matter.compiler

trait CompilationError
case class ParserError(msg: String) extends CompilationError
