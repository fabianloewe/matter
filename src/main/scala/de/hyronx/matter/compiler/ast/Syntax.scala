package de.hyronx.matter.compiler.ast

sealed trait SyntaxAST extends AST
case class Declaration(typeName: TypeName) extends SyntaxAST
case class VariableUsage(name: String) extends SyntaxAST
case class TypeConstruction(name: TypeName, genericTypes: Seq[TypeName] = Seq(), params: Seq[Variable] = Seq()) extends AST
case class Range(from: String, to: String) extends SyntaxAST

sealed trait EnclosingMultiple extends SyntaxAST {
  def definitions: Seq[AST]
}
object EnclosingMultiple {
  def unapply(encl: EnclosingMultiple) = Some(encl.definitions)
}

case class Concatenation(definitions: Seq[AST]) extends EnclosingMultiple
case class Selection(definitions: Seq[AST]) extends EnclosingMultiple

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
