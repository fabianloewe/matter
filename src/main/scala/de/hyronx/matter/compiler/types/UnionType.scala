package de.hyronx.matter.compiler.types

case class UnionType(wrappedTypes: Seq[Type]) extends CollectionType {
  val name = "Union"
  val maxIndex = MaxIndex(wrappedTypes.size)
}

case object UnionType extends CollectionType {
  val name = "Union"
  val wrappedTypes = List()
  val maxIndex = Infinite
}
