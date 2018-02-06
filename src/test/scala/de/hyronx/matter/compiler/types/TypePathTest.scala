package de.hyronx.matter.compiler.types

import org.scalatest._

class TypePathTest extends FlatSpec with Matchers {
  val path = AbsoluteTypePath(Stream("Base", "Test1", "Test2", "Test3"))

  "A type path" should "correctly find its parent" in {
    assert(path.getParent == RelativeTypePath(Stream("Test2")))
  }

  it should "correctly find its root" in {
    assert(path.getRoot == RelativeTypePath(Stream("Base")))
  }

  it should "equal the same paths" in {
    assert(RelativeTypePath(Stream("Base")) == RelativeTypePath(Stream("Base")))
  }
}
