package de.hyronx.matter.compiler.types

case class StringType(maxIndex: Index) extends CollectionType {
  val name = "String"
  val wrappedTypes = List(CharType)
}

case object StringType extends CollectionType {
  val name = "String"
  val wrappedTypes = List(CharType)
  val maxIndex = Infinite
}
