package de.hyronx.matter.compiler

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.tokens._
import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.parsers._

object Parser extends ContentParser with BehaviorParser {
  def defineContent: Parser[Content] = {
    openContainer ~ INDENT ~ content ^^ {
      case Identifier(name, family) ~ _ ~ cont if name == "Content" ⇒
        cont
    }
  }

  def defineBehavior: Parser[List[AST]] = {
    log(openContainer ~ INDENT ~ behavior)("Testing behavior") ^^ {
      case Identifier(name, family) ~ _ ~ behav if name == "Behavior" ⇒
        behav
    }
  }

  def program: Parser[ContainerTree] = {
    openContainer ~ rep(INDENT ~ openContainer) ~ INDENT >> {
      case first ~ list ~ _ ⇒
        val id: Identifier = list.map {
          case _ ~ container ⇒ container
        }.foldLeft(first) { (x, y) ⇒
          Identifier(y.name, x.family ::: x.name :: y.family)
        }

        BaseContainer.find(id) match {
          case Some(container) ⇒
            rep(createContainer(container)) ^^ { list ⇒
              list foreach { element ⇒ BaseContainer.children += element }
              BaseContainer
            }
          case None ⇒
            err(s"Container $id is not defined yet")
        }
    }
  }

  def createContainer(parent: ContainerTree): Parser[ContainerTree] = {
    newContainer ~ INDENT >> {
      case id ~ _ ⇒
        if (id.family.head == "Matter") {
          createMatter(id.name, parent) >> { initialized ⇒
            createContainer(initialized)
          }
        } else {
          createDescendant(id, parent)
        }
    } |
      assignContainer >> { id ⇒
        createDescendant(id, parent)
      }
  }

  def createMatter(name: String, parent: ContainerTree): Parser[Container] = {
    log(opt(defineContent ~ DEDENT) ~ opt(defineBehavior ~ DEDENT))("Create Matter") ^^ {
      case possibleContent ~ possibleBehavior ⇒
        val container = for {
          content ← possibleContent flatMap {
            case content ~ _ ⇒ Some(content.content)
          } orElse Some(Map.empty[String, Types.Definitions])

          behavior ← possibleBehavior flatMap {
            case behavior ~ _ ⇒ Some(behavior)
          } orElse Some(List.empty[AST])

        } yield Container(name, content, behavior, MatterContainer, parent)

        // yield returns an Option[Container] even though all options are catched
        container.get
    }
  }

  def createDescendant(id: Identifier, parent: ContainerTree): Parser[ContainerTree] = {
    val possibleContainer = {
      parent.find(Identifier(id.family.last, id.family.dropRight(1))) match {
        case Some(ancestor @ Container(_, content, behavior, _, _, _, _)) ⇒
          Some(Container(id.name, content, behavior, ancestor, parent))
        case Some(container) ⇒
          Some(PseudoContainer(id.name, container.ancestor, parent, container.children))
        case None ⇒
          None
      }
    }

    possibleContainer match {
      case Some(container) ⇒
        opt(INDENT) ~ (newContainer | assignContainer) >> {
          case Some(INDENT) ~ id ⇒
            createDescendant(id, container)
          case _ ~ id ⇒
            createDescendant(id, parent)
        } |
          openContainer >> { id ⇒ Executer(id, parent) }

      case None ⇒
        err(s"Container $id is not defined and can not be extended yet")
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

  def apply(
    tokens: Seq[Token]
  ): Either[ParserError, AST] = {
    val reader = new TokenReader(tokens)
    program(reader) match {
      case NoSuccess(msg, next) ⇒ Left(ParserError(msg + s" at ${next.first}"))
      case Success(result, next) ⇒
        println(s"Children: ${result.children.map(_.id)}")
        Right(result)
    }
  }
}
