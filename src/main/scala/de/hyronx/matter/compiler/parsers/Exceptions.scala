package de.hyronx.matter.compiler.parsers

import java.lang.RuntimeException

//import de.hyronx.matter.compiler.ast._

class VariableNotFound(varName: String) extends RuntimeException(
  "Variable \"" + varName + "\" could not be found"
)
