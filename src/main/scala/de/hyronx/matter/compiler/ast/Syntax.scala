package de.hyronx.matter.compiler.ast

trait SyntaxType {
  def name: String
}
object SyntaxType {
  case object String extends SyntaxType {
    val name = "String"
  }
  case object Grammar extends SyntaxType {
    val name = "Grammar"
  }
}

sealed trait SyntaxAST extends AST
case class Declaration(varType: SyntaxType) extends SyntaxAST
case class VariableUsage(name: String) extends SyntaxAST
case class Range(from: String, to: String) extends SyntaxAST

sealed trait EnclosingMultiple extends SyntaxAST {
  def definitions: AST.Definitions
}
object EnclosingMultiple {
  def unapply(encl: EnclosingMultiple) = Some(encl.definitions)
}

case class Concatenation(definitions: AST.Definitions) extends EnclosingMultiple
case class Selection(definitions: AST.Definitions) extends EnclosingMultiple

sealed trait Enclosing extends SyntaxAST {
  def definition: AST
}
object Enclosing {
  def unapply(encl: Enclosing) = Some(encl.definition)
}
case class Repeat(definition: AST) extends Enclosing
case class RepeatOne(definition: AST) extends Enclosing
case class Option(definition: AST) extends Enclosing
case class Group(definition: AST) extends Enclosing
