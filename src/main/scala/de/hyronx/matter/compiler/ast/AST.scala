package de.hyronx.matter.compiler.ast

import de.hyronx.matter.compiler.types.RelativeTypePath
import de.hyronx.matter.library.MutableTree

trait AST extends MutableTree[AST]

/* {
 override def children: Stream[AST] = throw new UnsupportedOperationException()
 override def addChild[R <: AST](child: R): R = throw new UnsupportedOperationException()
 override def addChildren(children: AST*): Unit = throw new UnsupportedOperationException()
} */
object AST {
  def apply[T <: AST](root: T, children: Stream[AST] = Stream()) = MutableTree(root, children: _*)
}

final class TypeName(_path: Stream[String]) extends RelativeTypePath(_path) with AST

object TypeName {
  def apply(path: Seq[String]) = new TypeName(path.toStream)

  def unapply(typeName: TypeName) = Some((typeName.path().last, typeName.path().dropRight(1)))
}

case class Literal(string: String) extends AST

case class Variable(name: String, varType: scala.Option[TypeName] = None) extends AST

case class TypeVariable(name: String, optType: scala.Option[TypeName] = None) extends AST
