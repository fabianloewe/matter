package de.hyronx.matter.compiler.validators

import scalaz.Tree

import de.hyronx.matter.compiler.Validator
import de.hyronx.matter.compiler.ast.AST

class ExpressionValidator(types: Tree[Validator#AbstractType], ast: Seq[AST]) {

}

object ExpressionValidator {
  def apply(types: Tree[Validator#AbstractType], ast: Seq[AST]) =
    new ExpressionValidator(types, ast)
}
