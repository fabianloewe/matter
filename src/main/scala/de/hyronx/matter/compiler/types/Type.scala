package de.hyronx.matter.compiler.types

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.ast.{ TypeName, VariableLike, BaseBuiltIn, MatterType }

case class Argument(
  val name: String,
  val index: Int,
  val varType: Type
) extends VariableLike

case class Method(
    name: String,
    args: List[Argument],
    retType: Type
) extends VariableLike {
  val varType = retType
}

trait Type {
  def name: String
  def members: Seq[VariableLike]

  def findMember(name: String) = members find (_.name == name)
}

object Type {
  val types: ListBuffer[Type] = ListBuffer(
    StringType,
    OptionalType,
    ListType,
    UnionType,
    TupleType,
    IntType,
    FloatType,
    BoolType,
    VoidType
  )

  def apply(name: String) = types find (_.name == name)
  def apply(name: TypeName) = types find (_.name == name.name) match {
    case None ⇒ BaseBuiltIn find name match {
      case Some(matterType: MatterType) ⇒ Some(StructType(matterType))
      case _                            ⇒ None
    }
    case found ⇒ found
  }
}
