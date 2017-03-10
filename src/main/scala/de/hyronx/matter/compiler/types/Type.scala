package de.hyronx.matter.compiler.types

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.ast.TypeName

class Argument(
  val name: String,
  val index: Int,
  val varType: Type
)

object Argument {
  def apply(name: String, index: Int, varType: Type) = new Argument(name, index, varType)
}

class Method(
  val name: String,
  val args: List[Argument],
  val retType: Type
)

object Method {
  def apply(name: String, args: List[Argument], retType: Type) = new Method(name, args, retType)
}

trait Type {
  def name: String
  def methods: List[Method]

  def findMethod(name: String): Option[Method] = methods find (_.name == name)
}

object Type {
  val types: ListBuffer[Type] = ListBuffer(
    StringType,
    OptionalType,
    ListType,
    TupleType,
    IntType,
    FloatType,
    BoolType,
    VoidType
  )

  def apply(name: String): Option[Type] = types find (_.name == name)
  def apply(name: TypeName): Option[Type] = types find (_.name == name.name)
}
