package de.hyronx.matter.compiler.ast

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.Parser
import de.hyronx.matter.compiler.parsers._

sealed trait MatterTypeTree extends AST {
  def id: String
  def ancestor: MatterTypeTree
  def descendants: ListBuffer[MatterTypeTree]
  def parent: MatterTypeTree
  def children: ListBuffer[MatterTypeTree]

  def equals(that: MatterTypeTree) = this.id == that.id
  def equals(that: String) = this.id == that
  def clone(parent: scala.Option[MatterTypeTree] = None): MatterTypeTree
  //def parser: fastparse.all.P[MatterTypeTree]

  def find(id: TypeName): scala.Option[MatterTypeTree] = {
    id.family.headOption match {
      case Some(idHead) ⇒
        if (this.id == idHead) {
          find(TypeName(id.name, id.family.tail))
        } else {
          this.children.find(_.id == idHead) match {
            case Some(matterType) ⇒
              // Try to find the next part of id.family
              matterType.find(TypeName(id.name, id.family.tail))
            case None ⇒
              // Check if there is no ancestor left
              if (this.ancestor == this) {
                None
              } else if (this.ancestor == MatterBuiltIn) {
                MatterBuiltIn.find(id)
              } else {
                this.ancestor.find(id)
              }
          }
        }
      case None ⇒
        if (this.id == id.name) {
          Some(this)
        } else {
          this.children.find(_.id == id.name) match {
            case Some(matterType) ⇒
              // There is no family defined so this must be the wanted matterType
              Some(matterType)
            case None ⇒
              // Check if a sibling with this name is defined
              parent.children.find(_.id == id.name) match {
                case Some(matterType) ⇒
                  Some(matterType)
                case None ⇒
                  // Check if there is no ancestor left
                  if (this.ancestor == this) {
                    None
                  } else if (this.ancestor == MatterBuiltIn) {
                    MatterBuiltIn.find(id)
                  } else {
                    this.ancestor.find(id)
                  }
              }
          }
        }
    }
  }
}

case object BaseBuiltIn extends MatterTypeTree {
  var id = "Base"
  val ancestor = this
  val descendants = ListBuffer.empty[MatterTypeTree]
  val parent = this
  val children = ListBuffer.empty[MatterTypeTree]

  def apply(id: String) = this.id = id
  def clone(parent: scala.Option[MatterTypeTree] = None): MatterTypeTree = this

  import fastparse.all._
  val parser = {
    val indent = Indentation(0)
    P(Parser.newContainer(this, indent) |
      Parser.openContainer(this, indent) |
      Parser.assignContainer(this, indent) |
      indent.same).log()
      .rep(1) ~ End map (_ ⇒ this)
  }
}

case class MatterType(
    id: String,
    syntax: AST.SyntaxMap,
    mappings: AST.Mappings,
    ancestor: MatterTypeTree,
    parent: MatterTypeTree,
    descendants: ListBuffer[MatterTypeTree],
    children: ListBuffer[MatterTypeTree]
) extends MatterTypeTree {
  import fastparse.all._

  addChildren(children)

  override def clone(parent: scala.Option[MatterTypeTree] = None): MatterTypeTree = {
    this.copy(parent = parent.getOrElse(this.parent))
  }

  private def addChildren(children: ListBuffer[MatterTypeTree]) = {
    this.children ++= MatterBuiltIn.children ++ children map { child ⇒
      child.clone(parent = Some(this))
    }
  }
}

object MatterType {
  def apply(
    id: String,
    syntax: AST.SyntaxMap,
    mappings: AST.Mappings,
    ancestor: MatterTypeTree,
    parent: MatterTypeTree,
    children: ListBuffer[MatterTypeTree] = ListBuffer()
  ): MatterType = {
    val matterType = new MatterType(
      id,
      syntax,
      mappings,
      ancestor,
      parent,
      ListBuffer(),
      children
    )
    ancestor.descendants += matterType
    parent.children += matterType
    matterType
  }

  def apply(
    id: String,
    ancestor: MatterType,
    parent: MatterTypeTree
  ): MatterType = {
    val matterType = new MatterType(id, ancestor.syntax, ancestor.mappings, ancestor, parent, ListBuffer(), ancestor.children)
    ancestor.descendants += matterType
    parent.children += matterType
    matterType
  }
}

case object MatterBuiltIn extends MatterTypeTree {
  val id = "Matter"
  val ancestor = this
  val descendants = ListBuffer()
  val parent = BaseBuiltIn
  // No new children allowed
  def children = ListBuffer(SyntaxBuiltIn(this), MappingBuiltIn(this))

  def clone(parent: scala.Option[MatterTypeTree] = None): MatterTypeTree = this
}

case class SyntaxBuiltIn(parent: MatterTypeTree) extends MatterTypeTree {
  val id = "Syntax"
  val ancestor = this
  val descendants = ListBuffer()
  def children = ListBuffer()

  def clone(parent: scala.Option[MatterTypeTree] = None): MatterTypeTree = {
    this.copy(parent = parent.getOrElse(this.parent))
  }
}

case class MappingBuiltIn(parent: MatterTypeTree) extends MatterTypeTree {
  val id = "Mapping"
  val ancestor = this
  val descendants = ListBuffer()
  def children = ListBuffer()

  def clone(parent: scala.Option[MatterTypeTree] = None): MatterTypeTree = {
    this.copy(parent = parent.getOrElse(this.parent))
  }
}
