package de.hyronx.matter.compiler.types

import org.scalatest._

class TypesTest extends FlatSpec with Matchers {
  "The standard types" should "correctly show up" in {
    Types.root.printTree(true, 0, true)
    succeed
  }
}
