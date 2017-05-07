package de.hyronx.matter.compiler.types

import de.hyronx.matter.compiler.ast.{ MatterType, Variable }

case class StructType(matterType: MatterType) extends Type {
  val name = matterType.id
  val members = matterType.mappings map { x ⇒ Variable(x.mappedVar, x.varType) }

  Type.types += this
}
