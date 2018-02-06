package de.hyronx.matter.compiler.types

import de.hyronx.matter.compiler.ast.AST
import de.hyronx.matter.library.MutableTree

trait TypeTrait extends MutableTree[TypeTrait] {
  self: TypeTrait ⇒
  def name: String

  def ancestor: TypeTrait

  def variables: Set[VariableLike]

  //def duplicate(): TypeTrait = self.asInstanceOf[MutableTree[TypeTrait]].duplicate()

  // Do not include the root part
  lazy val toJavaClass: String = path().drop(1).collect { case t: TypeTrait ⇒ t.name }.mkString("/")
  lazy val toJavaType: String = s"L$toJavaClass;"

  override def equals(that: Any): Boolean = that match {
    case that: TypeTrait ⇒ (name == that.name) && (ancestor == that.ancestor)
    /*
    case that: TypeName ⇒
      if (that.family.isEmpty) that.name == name
      else {
        /*
          val fullPath = typ.path.reverse.tail.map(_.name).toSeq
          println(s"TypeTraitEnhancer:isEqual! TypeName as path: $fullPath")
          (that.family :+ that.name) == fullPath
          */
        that == self
      }
    */
    case that: String    ⇒ that == name
    case that: TypePath  ⇒ that.toAbsolutePath == self.toPath
    case _               ⇒ false
  }

  override val hashCode: Int = {
    val prime = 17
    var hc = if (name != null) name.hashCode else 0
    hc = hc * prime + (if (ancestor != null) ancestor.hashCode else 0)
    hc
  }

  override def toString: String = toPath.toString

  def hasAncestor: Boolean = ancestor != null

  @scala.annotation.tailrec
  final def isAncestor(that: TypeTrait): Boolean = {
    if (ancestor == that)
      true
    else if (self.hasAncestor && ancestor.hasAncestor)
      ancestor.isAncestor(that)
    else
      false
  }

  def find[T <: TypeTrait](target: TypePath): Option[T] = root.find(_ == target).asInstanceOf[Option[T]]

  //private def isAbstractType(body: Seq[AST]) = body.isEmpty || body.exists(_.isInstanceOf[Declare])

  def toPath: TypePath = new AbsoluteTypePath(self.path().asInstanceOf[Stream[TypeTrait]], None)

  override def print(
    detailed: Boolean = false,
    indent: Int = 0,
    withSiblings: Boolean = true
  ): Unit = {
    def matchType[R <: TypeTrait](
      typ: R,
      list: Set[String] = Set()
    ): Set[String] = typ match {
      case _: AbstractTypeTrait if !list.contains("Abstract") ⇒ matchType(typ, list + "Abstract")
      case _: BuiltInTypeTrait if !list.contains("Built-in") ⇒ matchType(typ, list + "Built-in")
      case _: GenericTypeTrait if !list.contains("Generic") ⇒ matchType(typ, list + "Generic")
      case _: UserTypeTrait if !list.contains("Matter-based") ⇒ matchType(typ, list + "Matter-based")
      case _ ⇒ list
    }

    println(" " * indent + self.name)
    if (detailed) {
      println(" " * (indent + 2) + s"Type: ${matchType(self).mkString(", ")}")
      val typeString = self match {
        case genType: GenericTypeTrait ⇒
          println(" " * (indent + 4) + s"Generic bounds: ${genType.genericBounds.map(_.name).mkString}")
          if (!genType.implementations.isEmpty) {
            println(" " * (indent + 4) + s"Generic implementations:")
            genType.implementations.foreach(_.printTree(true, indent + 6, false))
          }
        case _ ⇒
      }

      if (self.ancestor != null)
        println(" " * (indent + 2) + s"Ancestor: ${self.ancestor.name}")
      if (!self.variables.isEmpty)
        println(" " * (indent + 2) + s"Variables: ${self.variables.mkString}")
    }
  }
}

object TypeTrait {
  def apply[T <: TypeTrait](root: T, children: TypeTrait*): T = MutableTree(root, children: _*)

  def unapply(typeTrait: TypeTrait) = Some((typeTrait.name, typeTrait.ancestor, typeTrait.variables, typeTrait.children, typeTrait.parent))
}
