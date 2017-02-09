package de.hyronx.matter.compiler

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.parsers._

object Parser extends ContentParser with BehaviorParser {
  import fastparse.all._

  def apply(
    code: String
  ): Either[ParserError, ContainerTree] = program.parse(code) match {
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
        println(s"Indent after openContainer: ${indent.indent}")
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
        println(s"Parent: $scoped")
        parent.find(scoped) match {
          case Some(MatterContainer) ⇒
            id
          case Some(ancestor @ Container(_, content, behavior, _, _, _, _)) ⇒
            Container(id, content, behavior, ancestor, parent)
          case Some(ancestor) ⇒
            PseudoContainer(id, ancestor, parent)
          case None ⇒
            None
        }
    } flatMap {
      case id: String ⇒
        newMatter(id, parent, indent)
      case container: ContainerTree ⇒
        val deeperIndent = Indentation(indent)
        P(newContainer(container, deeperIndent) |
          openContainer(container, deeperIndent) |
          assignContainer(container, deeperIndent))
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
    val newContent = expected("Content") flatMap { _ ⇒
      P(content(Indentation(bodyIndent)).log() ~/ bodyIndent.same) map { case (con, _) ⇒ con.content }
    }
    val newBehavior = expected("Behavior").log() flatMap { _ ⇒
      P(behavior ~/ bodyIndent.same) map { case (beh, _) ⇒ beh }
    }

    P(newContent.? ~ newBehavior.?).log() map {
      case (Some(content), Some(behavior)) ⇒
        Container(id, content, behavior.toList, MatterContainer, parent)
      case (_, _) ⇒
        PseudoContainer(id, MatterContainer, parent)
    } flatMap { container ⇒
      val deeperIndent = Indentation(indent)
      P(newContainer(container, deeperIndent) |
        openContainer(container, deeperIndent) |
        assignContainer(container, deeperIndent))
    }
  }

  // @param indent  Indentation of the header of the opened container
  def openContainer[T, V](
    parent: ContainerTree,
    indent: Indentation
  ): P[ContainerTree] = {
    P(scopedIdentifier ~ ":" ~/ indent.deeper).log() map {
      case (scoped, _) ⇒ parent.find(scoped)
    } flatMap {
      case Some(container) ⇒
        val deeperIndent = Indentation(indent)
        P(newContainer(container, deeperIndent) |
          openContainer(container, deeperIndent) |
          assignContainer(container, deeperIndent) /*|
          Executer(container) map { _ ⇒ parent }*/ )
      case None ⇒
        Fail
    }
  }

  def assignContainer(
    parent: ContainerTree,
    indent: Indentation
  ): P[ContainerTree] = {
    P(identifier ~ ws.rep(1) ~ "=" ~/ ws ~ scopedIdentifier ~
      (indent.same | indent.upper())).log() map {
      case (id, scoped: Identifier, newIndent) ⇒ parent.find(scoped) match {
        case Some(Container(_, content, behavior, ancestor, parent, _, children)) ⇒
          Some((Container(id, content, behavior, ancestor, parent, children), newIndent))
        case Some(twin) ⇒
          println(s"! assignContainer twin: ${twin.id}, parent: ${parent.id}")
          Some((PseudoContainer(id, twin.ancestor, parent, twin.children), newIndent))
        case None ⇒
          None
      }
    } flatMap {
      case Some((_, newIndent)) ⇒
        P(newContainer(parent, newIndent) |
          openContainer(parent, newIndent) |
          assignContainer(parent, newIndent))
      case None ⇒
        Fail
    }
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
