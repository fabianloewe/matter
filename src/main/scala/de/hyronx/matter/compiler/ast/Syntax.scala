package de.hyronx.matter.compiler.ast

import de.hyronx.matter.compiler.types.TypePath

//sealed trait SyntaxAST extends AST

case class Declaration(typeName: TypePath) extends AST

case class VariableUsage(name: String) extends AST

case class TypeConstruction(name: TypePath, genericTypes: Seq[TypePath] = Seq(), params: Seq[Variable] = Seq()) extends AST

case class Range(from: String, to: String) extends AST

case class Concatenation() extends BodyAST

case class Selection() extends BodyAST

sealed trait Enclosing extends AST {
  def definition: AST
}

object Enclosing {
  def unapply(encl: Enclosing) = Some(encl.definition)
}

case class Repeat(definition: AST) extends Enclosing

case class RepeatOne(definition: AST) extends Enclosing

case class Option(definition: AST) extends Enclosing

case class Group(definition: AST) extends Enclosing
