package de.hyronx.matter.compiler

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.parsers._

object Parser extends ContentParser with BehaviorParser {
  import fastparse.all._

  def apply(
    code: String
  ): Either[ParserError, ContainerTree] = BaseContainer.parser.parse(code) match {
    case fail: Parsed.Failure      ⇒ Left(ParserError(s"${fail.lastParser}"))
    case Parsed.Success(result, _) ⇒ Right(result)
  }

  val program: P[ContainerTree] = {
    // Multiple layers of containers may be opened
    // before the first one is newly defined.
    def openContainers(
      parent: ContainerTree = BaseContainer,
      indent: Indentation = Indentation(0)
    ): P[(ContainerTree, Indentation)] = {
      openContainer(parent, indent) flatMap { next ⇒
        openContainers(next, Indentation(indent)).? map {
          case Some((container, newIndent)) ⇒ (container, newIndent)
          case None                         ⇒ (next, indent)
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

  // @param indent  Indentation of the header of the new container
  def newContainer(
    parent: ContainerTree,
    indent: Indentation
  ): P[ContainerTree] = {
    P(identifier ~ " < " ~/ scopedIdentifier ~ ":" ~ indent.deeper).log() map {
      case (id, scoped: Identifier, _) ⇒
        if (scoped.name == MatterContainer.id) {
          id
        } else {
          parent.find(scoped) match {
            case Some(ancestor @ Container(_, content, behavior, _, _, _, _)) ⇒
              Container(id, content, behavior, ancestor, parent, ListBuffer())
            case _ ⇒
              None
          }
        }
    } flatMap {
      case id: String ⇒
        newMatter(id, parent, indent)
      case container: ContainerTree ⇒
        checkChild(container, Indentation(indent))
      case None ⇒
        Fail
    }
  }

  // @param indent  Indentation of the header of the new Matter container
  def newMatter(
    id: String,
    parent: ContainerTree,
    indent: Indentation
  ): P[ContainerTree] = {
    val bodyIndent = Indentation(indent)
    def expected(id: String) = P(id ~ ":" ~/ bodyIndent.deeper)
    val newContent = expected("Content").log() flatMap { _ ⇒
      content(Indentation(bodyIndent)) map { con ⇒ con.content }
    }
    val newBehavior = expected("Behavior") flatMap { _ ⇒
      behavior
    }

    P(newContent.? ~ (bodyIndent.same ~ newBehavior ~ bodyIndent.same.?).?).log() map {
      case (Some(content), Some((_, behavior, _))) ⇒
        Container(id, content, behavior.toList, MatterContainer, parent)
      case (_, _) ⇒
        PseudoContainer(id, MatterContainer, parent)
    } flatMap { container ⇒
      println(s"/ New Matter defined: ${container.id} with parent: ${parent.id}")
      checkChild(container, Indentation(indent)).log()
    }
  }

  // @param indent  Indentation of the header of the opened container
  def openContainer[T, V](
    parent: ContainerTree,
    indent: Indentation
  ): P[ContainerTree] = {
    P(scopedIdentifier ~ ":" ~/ indent.deeper).log() map {
      case (scoped, _) ⇒ parent.find(scoped) map (_.clone)
    } flatMap {
      case Some(container) ⇒
        container.parser
      /*case Some(container) ⇒
        checkChild(container, Indentation(indent))*/
      case None ⇒
        Fail
    }
  }

  def assignContainer(
    parent: ContainerTree,
    indent: Indentation
  ): P[ContainerTree] = {
    P(identifier ~ ws.rep(1) ~ "=" ~/ ws ~ scopedIdentifier ~
      (indent.same | indent.upper())) map {
      case (id, scoped: Identifier, newIndent) ⇒ parent.find(scoped) match {
        case Some(Container(_, content, behavior, ancestor, parent, _, children)) ⇒
          (Container(id, content, behavior, ancestor, parent, children), newIndent)
        case Some(ContentContainer) ⇒
          (ContentContainer(id, Indentation(indent), parent), newIndent)
        case Some(twin) ⇒
          println(s"! assignContainer twin: ${twin.id}, parent: ${parent.id}")
          (PseudoContainer(id, twin.ancestor, parent, twin.children), newIndent)
        case None ⇒
          None
      }
    } flatMap {
      case (_, newIndent: Indentation) ⇒ checkChild(parent, newIndent)
      case None                        ⇒ Fail
    }
  }

  def checkChild(parent: ContainerTree, indent: Indentation) = {
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
          list map { case container ~ dents ⇒ (container, dents) }
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
            element: (Container, Int) ⇒ element._1
          }.head
      }
  }*/
}
