package de.hyronx.matter.compiler.types

import java.lang.NumberFormatException

sealed trait Index
case class MaxIndex(value: Int) extends Index
case object Infinite extends Index

trait CollectionType extends Type {
  def wrappedTypes: Seq[Type]
  def maxIndex: Index

  val methods = List(
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

  override def findMethod(name: String) = {
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
          Some(Method(name, List(), retType))
        case Infinite ⇒
          Some(Method(name, List(), retType))
        case _ ⇒ None
      }
    } catch {
      case e: NumberFormatException ⇒
        methods find (_.name == name)
    }
  }
}
