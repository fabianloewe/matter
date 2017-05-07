package de.hyronx.matter.compiler.types

case object VoidType extends Type {
  val name = "Void"
  val members = List()
}

case object BoolType extends Type {
  val name = "Bool"
  val members = List()
}

case object CharType extends Type {
  val name = "Char"
  val members = List()
}

// This is a pseudo type to signal that the searched type could not be found yet.
case object UnknownType extends Type {
  val name = "Unknown"
  val members = List()
}
