package de.hyronx.matter.compiler.errors

class CompilationError(msg: String) extends RuntimeException(msg)

case class TypeError(msg: String) extends CompilationError(msg)

case class ParserError(msg: String) extends CompilationError(msg)

case class ValidatorError(msg: String) extends CompilationError(msg)

case class GeneratorError(msg: String) extends CompilationError(msg)
