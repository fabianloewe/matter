package de.hyronx.matter.compiler.parsers

import scala.collection.mutable.ListBuffer

import fastparse.core.{ ParserApi }

import de.hyronx.matter.compiler.tokens._
import de.hyronx.matter.compiler.ast.{
  AST,
  Identifier,
  Container,
  BaseContainer
}

class BaseParser extends fastparse.Api[Token, List[Token]](
  implicitly, TokenSetHelper, TokenReprOps, List.ordering
) {
  implicit def parserApi[T, V](p: T)(
    implicit
    c: T ⇒ core.Parser[V, Token, List[Token]]
  ): ParserApi[V, Token, List[Token]] = new FastparseAPI[V](p)

  def identifier: P[Identifier] = {
    P(IDENTIFIER ~ (DOT ~ IDENTIFIER).rep) map { (first, list) ⇒
      val result = first :: (list map { case _ ~ Identifier(next, _) ⇒ next })
      Identifier(result.last, result.dropRight(1))
    }
  }

  def newContainer: P[Identifier] = {
    simpleIdentifier ~ LESSTHAN ~ identifier ~ COLON map {
      case Identifier(name, _) ~ _ ~ Identifier(parent, rest) ~ _ ⇒
        Identifier(name, rest :+ parent)
    }
  }

  def openContainer: Parser[Identifier] = {
    log(identifier ~ COLON)("Open container") map {
      case id ~ _ ⇒ id
    }
  }

  def assignContainer: Parser[Identifier] = {
    simpleIdentifier ~ ASSIGN ~ identifier map {
      case Identifier(name, _) ~ _ ~ Identifier(assignee, rest) ⇒
        Identifier(name, rest :+ assignee)
    }
  }

  /*def processIndents(
    list: List[(Container, List[Token])],
    indentLevel: Int = 0
  ): List[(Container, Int)] = list.headOption match {
    case Some((container, dents)) ⇒
      println(s"Current indentLevel: $indentLevel for: $container")
      val (indents, dedents) = dents.partition(_ == INDENT)
      println(s"Indents: ${indents.size}, Dedents: ${dedents.size}\n")
      var nextIndentLevel = indentLevel + indents.size - dedents.size
      if (container.body.nonEmpty)
        nextIndentLevel = nextIndentLevel + 1

      (container, indentLevel) :: processIndents(list.tail, nextIndentLevel)
    case None ⇒ List()
  }*/
}
