package de.hyronx.matter.compiler.types

import org.scalatest._

class TypeTraitTest extends FlatSpec with Matchers {
  val base = BuiltInType("Base")

  "A type" should "correctly compare to a matching path" in {
    val typ = base.addChild(Type("Test", base, Seq()))
    val expectation = AbsoluteTypePath(Stream("Base", "Test"))

    assert(typ == expectation)
  }

  it should "correctly compare to its name" in {
    assert(base == "Base")
  }
}
