package de.hyronx.matter.compiler.types

case object VoidType extends Type {
  val name = "Void"
  val methods = List()
}

case object BoolType extends Type {
  val name = "Bool"
  val methods = List()
}

case object CharType extends Type {
  val name = "Char"
  val methods = List()
}

case class StructType(name: String) extends Type {
  val methods = List()
}
