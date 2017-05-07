package de.hyronx.matter.compiler.ast

import de.hyronx.matter.compiler.types.Type

trait AST
case class TypeName(name: String, var family: List[String] = List()) extends AST
case class ParamTypeName(`type`: TypeName, param: TypeName) extends AST
case class Literal(string: String) extends AST
// This becomes a Variable AST once 'type' is checked
trait VariableLike extends AST {
  def name: String
  def varType: Type
}
case class VariableDecl(name: String, typeName: TypeName) extends AST
case class Variable(name: String, varType: Type) extends VariableLike

object AST {
  type Definitions = Seq[AST]
  type SyntaxMap = collection.mutable.Map[String, AST]
  type Mappings = collection.mutable.ListBuffer[Mapping]
}
