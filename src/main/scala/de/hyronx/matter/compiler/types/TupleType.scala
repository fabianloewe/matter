package de.hyronx.matter.compiler.types

case class TupleType(wrappedTypes: Seq[Type]) extends CollectionType {
  val name = "Tuple"
  val maxIndex = MaxIndex(wrappedTypes.size - 1)
}

case object TupleType extends CollectionType {
  val name = "Tuple"
  val wrappedTypes = List()
  val maxIndex = Infinite
}
