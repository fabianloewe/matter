package de.hyronx.matter.compiler

import java.lang.RuntimeException

class CompilationError(msg: String) extends RuntimeException(msg)
case class ParserError(msg: String) extends CompilationError(msg)
case class GeneratorError(msg: String) extends CompilationError(msg)
