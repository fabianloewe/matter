package de.hyronx.matter.compiler.ast

import scala.collection.mutable.Set

import de.hyronx.matter.compiler.Parser
import de.hyronx.matter.compiler.parsers._

sealed trait MatterTypeTree extends AST {
  def id: String
  def ancestor: MatterTypeTree
  def descendants: Set[MatterTypeTree]
  def parent: MatterTypeTree
  def children: Set[MatterTypeTree]

  def clone(parent: scala.Option[MatterTypeTree] = None): MatterTypeTree
  def canEqual(a: Any) = a.isInstanceOf[MatterTypeTree]
  override def equals(that: Any): Boolean =
    that match {
      case that: String         ⇒ that == id
      case that: TypeName       ⇒ that.name.hashCode == id.hashCode
      case that: MatterTypeTree ⇒ that.canEqual(this) && this.hashCode == that.hashCode
      case _                    ⇒ false
    }
  override lazy val hashCode = {
    val prime = 31
    var result = 1
    result = prime * result + id.hashCode
    result = prime * result + ancestor.id.hashCode
    result
  }
  //def parser: fastparse.all.P[MatterTypeTree]

  def find(typeName: TypeName): scala.Option[MatterTypeTree] = {
    typeName.family.headOption match {
      case Some(idHead) ⇒
        if (this.id == idHead) {
          find(TypeName(typeName.name, typeName.family.tail))
        } else {
          this.children.find(_.id == idHead) match {
            case Some(matterType) ⇒
              // Try to find the next part of typeName.family
              matterType.find(TypeName(typeName.name, typeName.family.tail))
            case None ⇒
              // Check if there is no ancestor left
              if (this.ancestor == this) {
                None
              } else if (this.ancestor == MatterBuiltIn) {
                MatterBuiltIn.find(typeName)
              } else {
                this.ancestor.find(typeName)
              }
          }
        }
      case None ⇒
        if (this.id == typeName.name) {
          Some(this)
        } else {
          this.children.find(_.id == typeName.name) match {
            case Some(matterType) ⇒
              // There is no family defined so this must be the wanted matterType
              Some(matterType)
            case None ⇒
              // Check if a sibling with this name is defined
              parent.children.find(_.id == typeName.name) match {
                case Some(matterType) ⇒
                  Some(matterType)
                case None ⇒
                  // Check if there is no ancestor left
                  if (this.ancestor == this) {
                    None
                  } else {
                    this.ancestor.find(typeName)
                  }
              }
          }
        }
    }
  }
}

case class MatterType(
    id: String,
    syntax: AST.SyntaxMap,
    mappings: AST.Mappings,
    ancestor: MatterTypeTree,
    parent: MatterTypeTree,
    descendants: Set[MatterTypeTree],
    children: Set[MatterTypeTree],
    var isAbstract: Boolean
) extends MatterTypeTree {
  this.children ++= ancestor.children map (_.clone(Some(this)))
  println(s"MatterType:addChildren! Children: ${this.children map { x ⇒ (x.id, x.parent.id) }}")

  def clone(newParent: scala.Option[MatterTypeTree] = None) = {
    this.copy(parent = newParent.getOrElse(this.parent))
  }

  override def toString = {
    s"MatterType(id=$id)"
  }
}

object MatterType {
  def apply(
    id: String,
    syntax: AST.SyntaxMap,
    mappings: AST.Mappings,
    ancestor: MatterTypeTree,
    parent: MatterTypeTree,
    children: Set[MatterTypeTree] = Set(),
    isAbstract: Boolean = false
  ): MatterType = {
    val matterType = new MatterType(
      id,
      syntax,
      mappings,
      ancestor,
      parent,
      Set(),
      children,
      isAbstract
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
    val matterType = new MatterType(
      id,
      ancestor.syntax.clone,
      ancestor.mappings.clone,
      ancestor,
      parent,
      Set(),
      Set(),
      false
    )
    ancestor.descendants += matterType
    parent.children += matterType
    matterType
  }
}

case object BaseBuiltIn extends MatterTypeTree {
  var id = "Base"
  val ancestor = this
  val descendants = Set.empty[MatterTypeTree]
  val parent = this
  val children = Set.empty[MatterTypeTree]

  def apply(id: String) = {
    this.id = id
    this
  }
  def clone(newParent: scala.Option[MatterTypeTree] = None) = this

  import fastparse.all._
  val parser = {
    val indent = Indentation(0)
    P(Parser.newContainer(this, indent) |
      Parser.openContainer(this, indent) |
      Parser.assignContainer(this, indent) |
      indent.same)
      .rep(1) ~ End map (_ ⇒ this)
  }
}

case object MatterBuiltIn extends MatterTypeTree {
  val id = "Matter"
  val ancestor = this
  val descendants = Set.empty[MatterTypeTree]
  val parent = BaseBuiltIn
  // No new children allowed
  def children = Set(SyntaxBuiltIn(this), MappingBuiltIn(this))

  def clone(newParent: scala.Option[MatterTypeTree] = None) = this

  override def toString = {
    s"MatterBuiltIn(id=Matter)"
  }
}

case class SyntaxBuiltIn(parent: MatterTypeTree) extends MatterTypeTree {
  val id = "Syntax"
  val ancestor = this
  val descendants = Set.empty[MatterTypeTree]
  def children = Set()

  def clone(newParent: scala.Option[MatterTypeTree] = None) = {
    this.copy(parent = newParent.getOrElse(this.parent))
  }

  override def toString = {
    s"SyntaxBuiltIn(id=Syntax)"
  }
}

case class MappingBuiltIn(parent: MatterTypeTree) extends MatterTypeTree {
  val id = "Mapping"
  val ancestor = this
  val descendants = Set.empty[MatterTypeTree]
  def children = Set()

  def clone(newParent: scala.Option[MatterTypeTree] = None) = {
    this.copy(parent = newParent.getOrElse(this.parent))
  }

  override def toString = {
    s"MappingBuiltIn(id=Mapping)"
  }
}
