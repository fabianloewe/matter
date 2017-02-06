package de.hyronx.matter.compiler.ast

import scala.collection.mutable.ListBuffer

sealed trait ContainerTree extends AST {
  def id: String
  def ancestor: ContainerTree
  def descendants: ListBuffer[ContainerTree]
  def parent: ContainerTree
  def children: ListBuffer[ContainerTree]

  def equals(that: ContainerTree) = this.id == that.id
  def equals(that: String) = this.id == that

  def find(id: Identifier): scala.Option[ContainerTree] = {
    println(s"Searching in ${this.id}")
    id.family.headOption match {
      case Some(idHead) ⇒
        println(s"Search with family head $idHead")
        if (this.id == idHead) {
          find(Identifier(id.name, id.family.tail))
        } else {
          this.children.find(_.id == idHead) match {
            case Some(container) ⇒
              // Try to find the next part of id.family
              container.find(Identifier(id.name, id.family.tail))
            case None ⇒
              println("  Is not a child")
              // Check if there is no ancestor left
              if (this.ancestor == this) {
                println("    No ancestor left")
                None
              } else {
                this.ancestor.find(id)
              }
          }
        }
      case None ⇒
        println(s"No family head, searching for ${id.name}")
        if (this.id == id.name) {
          Some(this)
        } else {
          println(s"  Children: ${this.children map (_.id)}")
          this.children.find(_.id == id.name) match {
            case Some(container) ⇒
              println(s"    Container found: $container")
              // There is no family defined so this must be the wanted container
              Some(container)
            case None ⇒
              // Check if there is no ancestor left
              if (this.ancestor == this)
                None
              else
                this.ancestor.find(id)
          }
        }
    }
  }
}

case object BaseContainer extends ContainerTree {
  var id = "Base"
  val ancestor = this
  val descendants = ListBuffer.empty[ContainerTree]
  val parent = this
  val children = ListBuffer(MatterContainer)

  def apply(id: String) = this.id = id
}

case class PseudoContainer private (
  val id: String,
  val ancestor: ContainerTree,
  val parent: ContainerTree,
  val descendants: ListBuffer[ContainerTree],
  val children: ListBuffer[ContainerTree]
) extends ContainerTree

object PseudoContainer {
  def apply(
    id: String,
    ancestor: ContainerTree,
    parent: ContainerTree,
    children: ListBuffer[ContainerTree] = ListBuffer()
  ): PseudoContainer = new PseudoContainer(id, ancestor, parent, ListBuffer(), children)
}

case class Container private (
  val id: String,
  val content: Map[String, ContentAST],
  val behavior: List[AST],
  val ancestor: ContainerTree,
  val parent: ContainerTree,
  val descendants: ListBuffer[ContainerTree],
  val children: ListBuffer[ContainerTree]
) extends ContainerTree

object Container {
  def apply(
    id: String,
    content: Map[String, ContentAST],
    behavior: List[AST],
    ancestor: ContainerTree,
    parent: ContainerTree
  ): Container = {
    val container = new Container(id, content, behavior, ancestor, parent, ListBuffer(), ListBuffer())
    ancestor.descendants += container
    parent.children += container
    container
  }
}

case object MatterContainer extends ContainerTree {
  val id = "Matter"
  val ancestor = this
  val descendants = ListBuffer()
  val parent = BaseContainer
  val children = ListBuffer(ContentContainer, BehaviorContainer)
}

case object ContentContainer extends ContainerTree {
  val id = "Content"
  val ancestor = this
  val descendants = ListBuffer()
  val parent = MatterContainer
  val children = ListBuffer()
}

case object BehaviorContainer extends ContainerTree {
  val id = "Behavior"
  val ancestor = this
  val descendants = ListBuffer()
  val parent = MatterContainer
  val children = ListBuffer()
}
