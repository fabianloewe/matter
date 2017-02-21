package de.hyronx.matter.compiler.types

trait Type {
  def append(that: Type): Unit
}
