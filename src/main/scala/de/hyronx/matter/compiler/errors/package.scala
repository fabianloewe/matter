package de.hyronx.matter.compiler

import de.hyronx.matter.compiler.ast.Callee

package object errors {
  object TypeDoesNotMatch {
    def apply(varName: String, typ: Any) =
      s"The declared type of variable `$varName` does not match the inferred one `$typ`"
  }

  object UnknownVariable {
    def apply(varName: String) =
      s"Variable $varName is unknown"

    def apply(varName: String, forType: Any) =
      s"Variable `$varName` is unknown for `$forType`"
  }

  object InvalidFunctionCall {
    def apply(call: Callee) =
      s"Function call $call not validated"
  }
}
