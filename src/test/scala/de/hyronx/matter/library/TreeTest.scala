package de.hyronx.matter.library

import org.scalatest._

case class TreeImpl(val value: Int) extends MutableTree[TreeImpl]

class TreeTest extends FlatSpec with Matchers {
  val tree = MutableTree(
    new TreeImpl(0),
    MutableTree(
      new TreeImpl(1),
      new TreeImpl(2), new TreeImpl(3)),
    MutableTree(
      new TreeImpl(4),
      new TreeImpl(5)))

  "A tree" should "find a nested child" in {
    val target = 3
    val result = tree.find(_.value == target)
    assert(result.isDefined && result.get.value == target)
  }

  it should "add a child and find it afterwards" in {
    val value = 6
    tree.addChild(new TreeImpl(value))
    assert(tree.lastChild.isDefined && tree.lastChild.get.value == value)
  }

  it should "know it's root" in {
    assert(tree.isRoot)
  }

  it should "find its root" in {
    val target = 0
    val result = tree.find(_ == new TreeImpl(target))
    assert(result.isDefined && result.get.value == target)
  }

  it should "know if it's not the root" in {
    assert(!tree.children.head.isRoot)
  }

  it should "recognize its leafs" in {
    val target = 5
    val result = tree.find(_.value == target)
    assert(result.isDefined && result.get.isLeaf)
  }

  it should "build a path correctly" in {
    val target = 2
    val expectation = Stream(
      tree,
      tree.children.head,
      tree.children.head.children.head)
    val result = tree.find(_.value == target)
    assert(result.isDefined && result.get.path.equals(expectation))
  }

  it should "collect an element correctly" in {
    val target = tree.children.head.children.last
    val result = tree.collectFirst {
      case o@TreeImpl(x) if x == 3 ⇒ o
    }
    assert(result.isDefined && result.get == target)
  }

  it should "correctly iterate over each child" in {
    val target = List(1, 2, 3, 4, 5, 6)
    var result = List.empty[Int]

    tree.foreach { child ⇒ result = result :+ child.value }
    assert(result == target)
  }

  it should "correctly iterate over each child and itself" in {
    val target = List(0, 1, 2, 3, 4, 5, 6)
    var result = List.empty[Int]

    tree.foreach({ child ⇒ result = result :+ child.value }, true)
    assert(result == target)
  }

  /*
  it should "correctly replace a node" in {
    val target = 100
    val result = tree.find(_.value == 3).map { node =>
      node.replaceNode(node.clone(value = 100))
    }
  }
  *
  */
}
