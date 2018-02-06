package de.hyronx.matter.compiler.types

import de.hyronx.matter.compiler.ast.AST

sealed trait BuiltInTypeTrait extends TypeTrait {
  protected def anc: Option[TypeTrait] = None

  /* TODO: Likely insecure; consider different option */
  val ancestor: TypeTraitNode = anc.orNull
}

case class BuiltInType(
                        name: String,
                        override val anc: Option[TypeTrait] = None,
                        variables: Set[VariableLike] = Set()
                      ) extends BuiltInTypeTrait

case class PrimitiveBuiltInType(
                                 name: String,
                                 primitive: String,
                                 override val anc: Option[TypeTrait] = None,
                                 variables: Set[VariableLike] = Set()
                               ) extends BuiltInTypeTrait {
  override lazy val toJavaClass: String = s"java/lang/$primitive"
}

case class GenericBuiltInType(
                               name: String,
                               genericBounds: GenericParams,
                               override val anc: Option[TypeTrait] = None,
                               variables: Set[VariableLike] = Set(),
                               override val implementationInit: GenericTypeImpl => GenericTypeImpl = { x => x }
                             ) extends BuiltInTypeTrait with GenericTypeTrait

case class AbstractBuiltInType(
                                name: String,
                                override val anc: Option[TypeTrait] = None,
                                variables: Set[VariableLike] = Set()
                              ) extends AbstractTypeTrait with BuiltInTypeTrait

case class ExtendedBuiltInType(
                                name: String,
                                override val ancestor: TypeTrait,
                                var body: Seq[AST] = Seq(),
                                var variables: Set[VariableLike] = Set(),
                              ) extends UserTypeTrait with BuiltInTypeTrait
