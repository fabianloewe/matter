package de.hyronx.matter.compiler

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.parsers._

object Parser extends BaseParser {
  import fastparse.all._

  def apply(
    code: String
  ): Either[ParserError, MatterTypeTree] = BaseBuiltIn.parser.parse(code) match {
    case fail: Parsed.Failure      ⇒ Left(ParserError(s"${fail.lastParser}"))
    case Parsed.Success(result, _) ⇒ Right(result)
  }

  val program: P[MatterTypeTree] = {
    // Multiple layers of containers may be opened
    // before the first one is newly defined.
    def openContainers(
      parent: MatterTypeTree = BaseBuiltIn,
      indent: Indentation = Indentation(0)
    ): P[(MatterTypeTree, Indentation)] = {
      openContainer(parent, indent) flatMap { next ⇒
        openContainers(next, Indentation(indent)).? map {
          case Some((matterType, newIndent)) ⇒ (matterType, newIndent)
          case None                          ⇒ (next, indent)
        }
      }
    }

    openContainers() flatMap {
      case (parent, indent) ⇒
        (newContainer(parent, indent) | assignContainer(parent, indent)).rep(1) map { seq ⇒
          parent.children ++= seq
          parent
        }
    }
  }

  // @param indent  Indentation of the header of the new matterType
  def newContainer(
    parent: MatterTypeTree,
    indent: Indentation
  ): P[MatterTypeTree] = {
    P(typeName ~ " < " ~/ scopedType ~ ":" ~ indent.deeper).log() map {
      case (id, scoped: TypeName, _) ⇒
        if (scoped.name == MatterBuiltIn.id) {
          id
        } else {
          parent.find(scoped) match {
            case Some(ancestor: MatterType) ⇒
              MatterType(id, ancestor, parent)
            case _ ⇒
              None
          }
        }
    } flatMap {
      case id: String ⇒
        newMatter(id, parent, indent)
      case matterType: MatterTypeTree ⇒
        checkChild(matterType, Indentation(indent))
      case None ⇒
        Fail
    }
  }

  // @param indent  Indentation of the header of the new Matter matterType
  def newMatter(
    id: String,
    parent: MatterTypeTree,
    indent: Indentation
  ): P[MatterTypeTree] = {
    val bodyIndent = Indentation(indent)
    def expected(id: String) = P(id ~ ":" ~/ bodyIndent.deeper)

    // Check if Syntax is defined (required)
    expected("Syntax") flatMap { _ ⇒
      // It is. Parse the syntax definitions
      SyntaxParser(Indentation(bodyIndent))
    } flatMap { syntaxVars ⇒
      // Check if Mapping is defined (optional)
      (bodyIndent.same ~ expected("Mapping").log()).? flatMap {
        case Some(_) ⇒ MappingParser(syntaxVars, Indentation(bodyIndent))
        case None    ⇒ Pass
      } map {
        case Mappings(mappings) ⇒
          println(s"o New Mapping: $mappings")
          MatterType(
            id,
            syntaxVars,
            collection.mutable.ListBuffer(mappings: _*),
            MatterBuiltIn,
            parent
          )
        case other ⇒
          println(s"o No new mapping: $other")
          MatterType(
            id,
            syntaxVars,
            collection.mutable.ListBuffer(),
            MatterBuiltIn,
            parent
          )
      }
    }
  }

  // @param indent  Indentation of the header of the opened matterType
  def openContainer[T, V](
    parent: MatterTypeTree,
    indent: Indentation
  ): P[MatterTypeTree] = {
    P(scopedType ~ ":" ~/ indent.deeper).log() map {
      case (scoped, _) ⇒ parent.find(scoped) map (_.clone(None))
    } flatMap {
      case Some(syntax: SyntaxBuiltIn) if syntax.parent.isInstanceOf[MatterType] ⇒
        SyntaxParser(Indentation(indent)) map { syntaxVars ⇒
          println(s"o Syntax before: ${syntax.parent.asInstanceOf[MatterType].syntax}")
          syntax.parent.asInstanceOf[MatterType].syntax ++= syntaxVars
          println(s"o Syntax after: ${syntax.parent.asInstanceOf[MatterType].syntax}")
          syntax.parent
        }
      case Some(mapping: MappingBuiltIn) if mapping.parent.isInstanceOf[MatterType] ⇒
        val typeParent = mapping.parent.asInstanceOf[MatterType]
        MappingParser(typeParent.syntax, Indentation(indent)) map { mappings: Mappings ⇒
          println(s"o Mapping before: ${typeParent.mappings}")
          typeParent.mappings ++= mappings.mappings
          println(s"o Mapping after: ${typeParent.mappings}")
          mapping.parent
        }
      case Some(matterType) ⇒
        println(s"o openContainer: ${matterType.id}")
        checkChild(matterType, Indentation(indent))
      case None ⇒
        Fail
    }
  }

  def assignContainer(
    parent: MatterTypeTree,
    indent: Indentation
  ): P[MatterTypeTree] = {
    P(typeName ~ ws.rep(1) ~ "=" ~/ ws ~ scopedType ~
      (indent.same | indent.upper())) map {
      case (id, scoped: TypeName, newIndent) ⇒ parent.find(scoped) match {
        case Some(MatterType(_, content, behavior, ancestor, parent, _, children)) ⇒
          (MatterType(id, content, behavior, ancestor, parent, children), newIndent)
        /*case Some(twin) ⇒
          (PseudoContainer(id, twin.ancestor, parent, twin.children), newIndent)*/
        case _ ⇒
          None
      }
    } flatMap {
      case (_, newIndent: Indentation) ⇒ checkChild(parent, newIndent)
      case None                        ⇒ Fail
    }
  }

  def checkChild(parent: MatterTypeTree, indent: Indentation) = {
    P(newContainer(parent, indent) |
      openContainer(parent, indent) |
      assignContainer(parent, indent))
      .rep(sep = indent.same) map (_ ⇒ parent)
  }

  /*def firstContainer: Parser[AST] = {
    rep1(
      (newContainer | defineContent | defineBehavior | openContainer)
        ~ rep(INDENT | DEDENT)
    ) ^^ { list ⇒
        var parentTuple = parent → -1

        processIndents(
          list map { case matterType ~ dents ⇒ (matterType, dents) }
        ).scanLeft(parentTuple) { (left, right) ⇒
            //println(s"Comparing $left vs $right\n")
            left._2 match {
              case i: Int if i == right._2 ⇒
                parentTuple._1.body += (left._1, right._1)
              case i: Int if i < right._2 ⇒
                left._1.body += right._1
                parentTuple = left
              case i: Int if i > right._2 ⇒
                right._1.body += left._1
                parentTuple = right
            }
            right
          }.map {
            element: (MatterType, Int) ⇒ element._1
          }.head
      }
  }*/
}
