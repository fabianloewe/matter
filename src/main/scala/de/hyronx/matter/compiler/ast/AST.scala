package de.hyronx.matter.compiler.ast

trait AST
case class TypeName(name: String, var family: List[String] = List()) extends AST
case class Literal(string: String) extends AST
case class Variable(name: String, varType: scala.Option[String] = None) extends AST
case class TypeVariable(name: String, ancestor: scala.Option[TypeName] = None) extends AST

case class TypeDefinition(
  name: TypeName,
  ancestor: TypeName,
  typeDecls: Seq[TypeVariable],
  params: Seq[Variable],
  body: Seq[AST]
) extends AST

case class TypeReopening(
  name: TypeName,
  typeDecls: Seq[TypeName],
  params: Seq[Variable],
  body: Seq[AST]
) extends AST

case class TypeDuplication(
  name: TypeName,
  twinName: TypeName,
  typeDecls: Seq[TypeVariable],
  setParams: Seq[Variable]
) extends AST

/*
trait VariableLike extends AST {
  def name: String
  def varType: Type
}
case class VariableDecl(name: String, typeName: TypeName) extends AST
case class Variable(name: String, varType: Type) extends VariableLike
case class Variables(variables: Seq[VariableLike]) extends AST
*/
