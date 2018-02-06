package de.hyronx.matter.compiler.ast

import de.hyronx.matter.library.MutableTree

trait BodyAST extends AST

/* {
 override def children: Stream[AST] = super[MutableTree]._children
 override def addChild[R <: AST](child: R): R = super[MutableTree].addChild(child)
 override def addChildren(children: AST*): Unit = super[MutableTree].addChildren(children: _*)
} */
object BodyAST {
  def unapply(bodyAST: BodyAST) = Some((bodyAST, bodyAST.children))
}

case class TypeDefinition(
                           name: TypeName, // Name of type
                           ancestor: TypeName, // Ancestor
                           typeDecls: Seq[TypeVariable], // Generic type parameters/declarations
                           params: Seq[Variable] // Instantiation parameters
                         ) extends BodyAST // Body containing other type definitions etc.

case class TypeReopening(
                          name: TypeName,
                          typeDecls: Seq[TypeName],
                          params: Seq[Variable]
                        ) extends BodyAST

case class TypeDuplication(
                            name: TypeName,
                            twinName: TypeName,
                            typeDecls: Seq[TypeVariable],
                            setParams: Seq[Variable]
                          ) extends AST