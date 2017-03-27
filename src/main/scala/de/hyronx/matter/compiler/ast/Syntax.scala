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
case class Concatenation(definitions: AST.Definitions) extends AST
case class Range(from: Char, to: Char) extends SyntaxAST
case class Selection(options: AST.Definitions) extends SyntaxAST
case class Repeat(definition: AST) extends SyntaxAST
case class RepeatOne(definition: AST) extends SyntaxAST
case class Option(definition: AST) extends SyntaxAST
case class Group(definition: AST) extends SyntaxAST
