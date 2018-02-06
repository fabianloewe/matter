package de.hyronx.matter.compiler.types

import de.hyronx.matter.compiler.ast.AST

trait UserTypeTrait extends TypeTrait {
  var body: Seq[AST]
  var variables: Set[VariableLike]

  def hasSyntax(): Boolean = getSyntax().isDefined

  def getSyntax(): Option[UserTypeTrait] = findChild { child ⇒
    child == "Syntax" && !child.isInstanceOf[AbstractTypeTrait]
  } collectFirst { case child: UserTypeTrait ⇒ child }

  def hasMapping(): Boolean = getMapping().isDefined

  def getMapping(): Option[UserTypeTrait] = findChild { child ⇒
    child == "Mapping" && !child.isInstanceOf[AbstractTypeTrait]
  } collectFirst { case child: UserTypeTrait ⇒ child }
}

object UserTypeTrait {
  def unapply(typeTrait: UserTypeTrait) = Some((
    typeTrait.name,
    typeTrait.ancestor,
    typeTrait.variables,
    typeTrait.body,
    typeTrait.children,
    typeTrait.parent
  ))
}

case class Type(
                 name: String,
                 ancestor: TypeTrait,
                 var body: Seq[AST],
                 var variables: Set[VariableLike] = Set()
               ) extends UserTypeTrait

trait AbstractTypeTrait extends TypeTrait

case class AbstractType(
                         name: String,
                         ancestor: TypeTrait,
                         var body: Seq[AST] = Seq(),
                         var variables: Set[VariableLike] = Set()
                       ) extends AbstractTypeTrait with UserTypeTrait
