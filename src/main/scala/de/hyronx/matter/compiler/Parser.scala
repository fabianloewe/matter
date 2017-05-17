package de.hyronx.matter.compiler

import de.hyronx.matter.Config
import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.parsers._

object Parser extends BaseParser {
  import fastparse.all._

  def apply(
    code: String
  )(
    implicit
    config: Config
  ): Either[ParserError, MatterTypeTree] = BaseBuiltIn(config.packageName).parser.parse(code) match {
    case fail: Parsed.Failure      ⇒ Left(ParserError(s"${fail.lastParser}"))
    case Parsed.Success(result, _) ⇒ Right(result)
  }

  // @param indent  Indentation of the header of the new matterType
  def newContainer(
    parent: MatterTypeTree,
    indent: Indentation
  ): P[MatterTypeTree] = {
    P(typeName ~ " < " ~/ scopedType ~ ":" ~ indent.deeper) map {
      case (id, scoped: TypeName, _) if MatterBuiltIn == scoped ⇒ id
      case (id, scoped: TypeName, _) ⇒ parent.find(scoped) match {
        case Some(ancestor: MatterType) ⇒
          println(s"Parser:newContainer! New matter type's parent: $parent")
          MatterType(id, ancestor, parent)
        case _ ⇒ None
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
      // Check if there are any declared variables
      // If so the type should be abstract
      val isAbstract = syntaxVars exists (_._2.isInstanceOf[Declaration])
      // Check if Mapping is defined (optional)
      (bodyIndent.same ~ expected("Mapping")).? flatMap {
        case Some(_) ⇒ MappingParser(syntaxVars, Indentation(bodyIndent))
        case None    ⇒ Pass
      } map {
        case Mappings(mappings) ⇒
          println(s"Parser:newMatter! New Mapping: $mappings")
          MatterType(
            id,
            syntaxVars,
            collection.mutable.ListBuffer(mappings: _*),
            MatterBuiltIn,
            parent,
            isAbstract = isAbstract
          )
        case other ⇒
          println(s"Parser:newMatter! No new mapping: $other")
          MatterType(
            id,
            syntaxVars,
            collection.mutable.ListBuffer(),
            MatterBuiltIn,
            parent,
            isAbstract = isAbstract
          )
      }
    }
  }

  // @param indent  Indentation of the header of the opened matterType
  def openContainer(
    parent: MatterTypeTree,
    indent: Indentation
  ): P[MatterTypeTree] = {
    P(scopedType ~ ":" ~/ indent.deeper) map {
      case (scoped, _) ⇒
        println(s"Parser:openContainer! Parent: ${parent.id}")
        parent.find(scoped) map (_.clone(None))
    } flatMap {
      case Some(syntax: SyntaxBuiltIn) if syntax.parent.isInstanceOf[MatterType] ⇒
        val typeParent = syntax.parent.asInstanceOf[MatterType]
        SyntaxParser(Indentation(indent)) map { syntaxVars ⇒
          println(s"Parser:openContainer! Syntax before: ${typeParent.syntax}")
          typeParent.syntax ++= syntaxVars
          println(s"Parser:openContainer! Syntax after: ${typeParent.syntax}")
          typeParent.isAbstract = false
          syntax.parent
        }
      case Some(mapping: MappingBuiltIn) if mapping.parent.isInstanceOf[MatterType] ⇒
        val typeParent = mapping.parent.asInstanceOf[MatterType]
        MappingParser(typeParent.syntax, Indentation(indent)) map {
          case Mappings(mappings) ⇒
            println(s"Parser:openContainer! Mapping before: ${typeParent.mappings} for: ${typeParent.id}")
            typeParent.mappings ++= mappings
            println(s"Parser:openContainer! Mapping after: ${typeParent.mappings}")
            mapping.parent
        }
      case Some(matterType) ⇒
        println(s"Parser:openContainer! No mapping or syntax for ${matterType.id}")
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
        case Some(MatterType(_, content, behavior, ancestor, parent, _, children, isAbstract)) ⇒
          (MatterType(id, content, behavior, ancestor, parent, children, isAbstract), newIndent)
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
