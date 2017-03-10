package de.hyronx.matter.compiler.types

case class ListType(wrappedType: Type) extends CollectionType {
  val name = "List"
  val wrappedTypes = List(wrappedType)
  val maxIndex = Infinite
}

case object ListType extends CollectionType {
  val name = "List"
  val wrappedTypes = List()
  val maxIndex = Infinite
}
