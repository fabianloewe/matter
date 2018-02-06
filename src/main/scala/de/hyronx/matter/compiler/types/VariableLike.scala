package de.hyronx.matter.compiler.types

import de.hyronx.matter.compiler.ast.AST
import de.hyronx.matter.compiler.errors.TypeError

/* TODO: `varType` should not be optional */
sealed trait VariableLike {
  def name: String

  def varType: TypeTrait
}

object VariableLike {
  def unapply(varLike: VariableLike): Option[(String, TypeTrait)] = Some((varLike.name, varLike.varType))
}

case class Variable[T <: TypeTrait](name: String, varType: T, assignedAst: Option[AST] = None) extends VariableLike

case class JVMVariable(name: String, varType: TypeTrait, jvmIndex: Int) extends VariableLike

case class IndexAccessor(index: Int, varType: TypeTrait, parentType: GenericTypeImpl) extends VariableLike {
  val name = index.toString

  override def equals(that: Any) = that match {
    case IndexAccessor(thatIndex, thatVarType, thatParentType) ⇒
      index == thatIndex && varType == thatVarType && parentType == thatParentType
    case that: String ⇒
      parentType.basedOn == Types.tuple && name == that
  }
}
