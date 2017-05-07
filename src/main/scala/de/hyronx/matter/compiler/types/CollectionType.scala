package de.hyronx.matter.compiler.types

import java.lang.NumberFormatException

import de.hyronx.matter.compiler.ast.Variable

sealed trait Index
case class MaxIndex(value: Int) extends Index
case object Infinite extends Index

trait CollectionType extends Type {
  def wrappedTypes: Seq[Type]
  def maxIndex: Index

  val members = List(
    Method("not-empty", List(), BoolType),

    Method("contains", List(
      Argument("that", 0, this)
    ), BoolType),

    Method("find", List(
      Argument("that", 0, this)
    ), this),

    Method("replace", List(
      Argument("that", 0, this),
      Argument("with", 1, this),
      Argument("at", 2, IntType)
    ), VoidType)
  )

  override def findMember(name: String) = {
    try {
      val index = name.toInt
      val retType = {
        if (wrappedTypes isDefinedAt index)
          wrappedTypes(index)
        else
          wrappedTypes.head
      }

      maxIndex match {
        case MaxIndex(maxIndexVal) if (index <= maxIndexVal) ⇒
          Some(Variable(name, retType))
        case Infinite ⇒
          Some(Variable(name, retType))
        case _ ⇒ None
      }
    } catch {
      case e: NumberFormatException ⇒ members find (_.name == name)
    }
  }
}
