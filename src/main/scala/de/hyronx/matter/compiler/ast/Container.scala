package de.hyronx.matter.compiler.ast

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.Parser
import de.hyronx.matter.compiler.Parser.Indentation
import de.hyronx.matter.compiler.engine.ParserGenerator

sealed trait ContainerTree extends AST {
  def id: String
  def ancestor: ContainerTree
  def descendants: ListBuffer[ContainerTree]
  def parent: ContainerTree
  def children: ListBuffer[ContainerTree]

  def equals(that: ContainerTree) = this.id == that.id
  def equals(that: String) = this.id == that
  override def clone: ContainerTree = this
  def parser: fastparse.all.P[ContainerTree]

  def find(id: Identifier): scala.Option[ContainerTree] = {
    println(s"* Searching in '${this.id}' for $id")
    id.family.headOption match {
      case Some(idHead) ⇒
        println(s"* Searching for family head '$idHead'")
        if (this.id == idHead) {
          println("* Family head found")
          find(Identifier(id.name, id.family.tail))
        } else {
          this.children.find(_.id == idHead) match {
            case Some(container) ⇒
              println("* Family head found in children")
              // Try to find the next part of id.family
              container.find(Identifier(id.name, id.family.tail))
            case None ⇒
              println(s"* Current head is not a child of '${this.id}'")
              // Check if there is no ancestor left
              if (this.ancestor == this) {
                println(s"* Fail! Current: ${this.id}")
                None
              } else if (this.ancestor == MatterContainer) {
                MatterContainer.find(id)
              } else {
                this.ancestor.find(id)
              }
          }
        }
      case None ⇒
        println("* No family head")
        if (this.id == id.name) {
          println("* Name found")
          Some(this)
        } else {
          println(s"* Children of '${this.id}': ${this.children map (_.id)}")
          this.children.find(_.id == id.name) match {
            case Some(container) ⇒
              println(s"* Container found in children")
              // There is no family defined so this must be the wanted container
              Some(container)
            case None ⇒
              // Check if a sibling with this name is defined
              println(s"* Parent '${parent.id}' children: ${parent.children map (_.id)}")
              parent.children.find(_.id == id.name) match {
                case Some(container) ⇒
                  println("* Container found in parent")
                  Some(container)
                case None ⇒
                  // Check if there is no ancestor left
                  if (this.ancestor == this) {
                    println(s"* Fail! Current: ${this.id}")
                    None
                  } else if (this.ancestor == MatterContainer) {
                    MatterContainer.find(id)
                  } else {
                    this.ancestor.find(id)
                  }
              }
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
  val children = ListBuffer.empty[ContainerTree]

  def apply(id: String) = this.id = id

  import fastparse.all._
  val parser = {
    val indent = Indentation(0)
    P(Parser.newContainer(this, indent) |
      Parser.openContainer(this, indent) |
      Parser.assignContainer(this, indent) |
      indent.same).rep(1) ~ End map (_ ⇒ this)
  }
}

case class PseudoContainer private (
    val id: String,
    val ancestor: ContainerTree,
    val parent: ContainerTree,
    val descendants: ListBuffer[ContainerTree],
    val children: ListBuffer[ContainerTree]
) extends ContainerTree {
  override def clone: ContainerTree = this.copy()

  import fastparse.all._
  val parser = Pass map (_ ⇒ this)
}

object PseudoContainer {
  def apply(
    id: String,
    ancestor: ContainerTree,
    parent: ContainerTree,
    children: ListBuffer[ContainerTree] = ListBuffer()
  ): PseudoContainer = {
    val container = new PseudoContainer(id, ancestor, parent, ListBuffer(), children)
    ancestor.descendants += container
    parent.children += container
    container
  }
}

case class Container private (
    id: String,
    contentMap: Types.ContentMap,
    behavior: List[AST],
    ancestor: ContainerTree,
    parent: ContainerTree,
    descendants: ListBuffer[ContainerTree],
    children: ListBuffer[ContainerTree]
) extends ContainerTree {
  import fastparse.all._

  val variables = scala.collection.mutable.Map.empty[String, Any]
  lazy val parserFunc = ParserGenerator(contentMap, this)
  def parser = {
    parserFunc().log() map { x ⇒
      println(s"parser(): $x")
      variables ++= x
      this
    }
  }

  override def clone: ContainerTree = this.copy()
}

object Container {
  def apply(
    id: String,
    content: Types.ContentMap,
    behavior: List[AST],
    ancestor: ContainerTree,
    parent: ContainerTree,
    children: ListBuffer[ContainerTree] = ListBuffer()
  ): Container = {
    val container = new Container(id, content, behavior, ancestor, parent, ListBuffer(), children)
    ancestor.descendants += container
    parent.children += container
    container
  }
}

case class ContentContainer(
    id: String,
    indent: Indentation,
    parent: ContainerTree
) extends ContainerTree {
  val ancestor = MatterContainer
  val descendants = ListBuffer()
  // No new children currently allowed
  // because of clashes with the content
  def children = ListBuffer()
  parent.children += this

  import fastparse.all._
  val variables = scala.collection.mutable.Map.empty[String, Any]
  var parser = {
    Parser.content(indent) map {
      case Content(contentMap) ⇒
        parser = ParserGenerator(contentMap, this)() map { x ⇒
          println(s"parser(): $x")
          variables ++= x
          this.asInstanceOf[ContainerTree]
        }
        this
    }
  }
}

case object MatterContainer extends ContainerTree {
  val id = "Matter"
  val ancestor = this
  val descendants = ListBuffer()
  val parent = BaseContainer
  // No new children allowed
  def children = ListBuffer(ContentContainer, BehaviorContainer)

  val parser = {
    import fastparse.all._
    Pass map (_ ⇒ this)
  }
}

case object ContentContainer extends ContainerTree {
  val id = "Content"
  val ancestor = this
  // No descendants allowed
  def descendants = ListBuffer()
  val parent = MatterContainer
  // No new children allowed
  def children = ListBuffer()

  val parser = {
    import fastparse.all._
    Pass map (_ ⇒ this)
  }
}

case object BehaviorContainer extends ContainerTree {
  val id = "Behavior"
  val ancestor = this
  // No descendants allowed
  def descendants = ListBuffer()
  val parent = MatterContainer
  // No new children allowed
  def children = ListBuffer()

  val parser = {
    import fastparse.all._
    Pass map (_ ⇒ this)
  }
}
