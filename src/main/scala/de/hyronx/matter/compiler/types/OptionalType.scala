package de.hyronx.matter.compiler.types

case class OptionalType(wrappedType: Type) extends CollectionType {
  val name = "Optional"
  val wrappedTypes = List(wrappedType)
  val maxIndex = MaxIndex(0)
}

case object OptionalType extends CollectionType {
  val name = "Optional"
  val wrappedTypes = List()
  val maxIndex = MaxIndex(0)
}
