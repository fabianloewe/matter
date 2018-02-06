package de.hyronx.matter.compiler.types

import scala.collection.immutable.SortedSet

import scalaz.TreeLoc

import de.hyronx.matter.compiler.ast.AST

sealed trait GenericParameterTrait {
  def name: String
}

object GenericParameterTrait {
  def unapply(genParam: GenericParameterTrait) = Some(genParam.name)
}

// Describes a generic paramter with one possible type
case class SingleGenericParameter(name: String, genType: TypeTraitNode) extends GenericParameterTrait

// Describes a generic paramter with multiple possible types
case class UnionGenericParameter(name: String, genTypes: Set[TypeTraitNode]) extends GenericParameterTrait

// Describes a generic parameter with one possible type but it can occure `maxNumber` times (like in a tuple)
case class VariadicGenericParameter(name: String, genType: Seq[TypeTraitNode]) extends GenericParameterTrait

// Describes a generic parameter which can be any of the above
/* TODO: Sense? */
case class AnyGenericParameter(name: String) extends GenericParameterTrait

trait GenericTypeTrait extends TypeTrait {
  self ⇒
  def genericBounds: GenericParams

  // Every GenericParams must fulfill the `genericBounds`
  private[types] var _implementations = Set.empty[GenericTypeImpl]

  def implementations: Set[GenericTypeImpl] = _implementations

  protected val implementationInit: GenericTypeImpl ⇒ GenericTypeImpl = { x ⇒ x }

  private def checkGenericParams(genParams: GenericParams): Boolean = genParams.map { genParam ⇒
    (genParam, self.genericBounds.find(genParam.name == _.name))
  }.forall {
    case (_, None) ⇒ false
    case (SingleGenericParameter(_, usedType), Some(SingleGenericParameter(_, boundType))) ⇒ usedType.isAncestor(boundType)
    case (UnionGenericParameter(_, usedTypes), Some(UnionGenericParameter(_, boundTypes))) ⇒ usedTypes.zip(boundTypes).forall {
      case (usedType, boundType) ⇒ usedType.isAncestor(boundType)
    }
    case (VariadicGenericParameter(_, usedTypes), Some(VariadicGenericParameter(_, boundTypes))) ⇒ usedTypes.zip(boundTypes).forall {
      case (usedType, boundType) ⇒ usedType.isAncestor(boundType)
    }
    case _ ⇒ false
  }

  def addImplementation(
    genParams: GenericParams,
    variables: Set[VariableLike],
    body: Seq[AST]
  ): Option[GenericTypeImpl] = {
    if (checkGenericParams(genParams)) {
      val genImpl = GenericTypeImpl(
        self,
        self.name + genParams.map {
          case SingleGenericParameter(_, single: TypeTraitNode) ⇒
            "$$" + single.name + "$$"
          case UnionGenericParameter(_, multiple: Set[TypeTraitNode]) ⇒
            "$$" + multiple.map(_.name).mkString("_") + "$$"
          case VariadicGenericParameter(_, multiple: Seq[TypeTraitNode]) ⇒
            "$$" + multiple.map(_.name).mkString("~") + "$$"
        }.mkString("$"),
        self.ancestor,
        genParams,
        variables,
        body
      )

      genImpl.addChildren(children.map(_.duplicate()): _*)

      _implementations = implementations + genImpl
      parent.map(_.addChild(genImpl)).orElse(Some(genImpl)).map(implementationInit(_))
    } else None
  }

  def getImplementation(
    genParams: GenericParams,
    body: Seq[AST] = Seq()
  ): Option[GenericTypeImpl] = {
    self.implementations.find(_.genericBounds == genParams).orElse {
      self.addImplementation(
        genParams,
        self.variables,
        body
      )
    }
  }

  def getImplementation(
    types: Seq[TypeTrait]
  ): Option[GenericTypeImpl] = {
    val genParams: GenericParams = genericBounds.map(_.name).zip(types).map {
      case (name, typ) ⇒ SingleGenericParameter(name, typ)
    }
    getImplementation(genParams)
  }
}

object GenericTypeTrait {
  def unapply(genType: GenericTypeTrait) = Some((
    genType.name,
    genType.ancestor,
    genType.variables,
    genType.genericBounds,
    genType.implementations,
    genType.children,
    genType.parent
  ))
}

trait GenericUserTypeTrait extends GenericTypeTrait with UserTypeTrait {
  self ⇒
  var genericBounds: GenericParams

  // An override is needed because the bodies must be concatenated
  override def addImplementation(
    bounds: GenericParams,
    variables: Set[VariableLike],
    body: Seq[AST]
  ): Option[GenericTypeImpl] = {
    super.addImplementation(bounds, variables, self.body ++ body)
  }

  // Another override to include body
  override def getImplementation(
    genParams: GenericParams,
    body: Seq[AST] = Seq()
  ): Option[GenericTypeImpl] = super.getImplementation(genParams, self.body ++ body)
}

case class GenericType(
  name: String,
  ancestor: TypeTrait,
  var body: Seq[AST],
  var genericBounds: GenericParams = GenericParams(),
  var variables: Set[VariableLike] = Set()
) extends GenericUserTypeTrait

case class GenericTypeImpl(
  basedOn: GenericTypeNode,
  name: String,
  ancestor: TypeTrait,
  genericBounds: GenericParams,
  var variables: Set[VariableLike],
  var body: Seq[AST] = Seq()
) extends UserTypeTrait

case class AbstractGenericType(
  name: String,
  ancestor: TypeTrait,
  var body: Seq[AST],
  var genericBounds: GenericParams = GenericParams(),
  var variables: Set[VariableLike] = Set()
) extends GenericUserTypeTrait with AbstractTypeTrait
