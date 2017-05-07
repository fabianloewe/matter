package de.hyronx.matter.compiler.types

trait NumeralType extends Type {
  val members = List()
}

case object IntType extends NumeralType {
  val name = "Int"
}

case object FloatType extends NumeralType {
  val name = "Float"
}
