package de.hyronx.matter.compiler.types

import org.scalatest._

import de.hyronx.matter.library.MutableTree

class GenericTypeTraitTest extends FlatSpec with Matchers {
  val base = MutableTree(
    BuiltInType("Base"),
    BuiltInType("Matter"))

  val genericBounds = GenericParams(
    SingleGenericParameter("T", base.getChild(0).get))

  base.addChild(
    GenericBuiltInType(
      "Test",
      genericBounds))

  "A generic type" should "correctly add an implementation" in {
    val target = "Test"
    val basedOn = base.find(_.name == target).collectFirst { case x: GenericTypeTrait ⇒ x }
    val result = basedOn.flatMap {
      case generic: GenericTypeTrait ⇒ generic.addImplementation(genericBounds, Set(), Seq())
    }

    assert(result.isDefined && result.get.parent == Some(base) && basedOn.get.implementations.find(_ == result.get).isDefined)
  }
}
