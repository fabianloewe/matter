package de.hyronx.matter.compiler.parsers

import scala.collection.mutable.ListBuffer
import scala.util.parsing.input._
import scala.util.parsing.combinator.Parsers

import de.hyronx.matter.compiler.tokens._
import de.hyronx.matter.compiler.ast.{
  Identifier,
  Literal,
  Container,
  BaseContainer
}

class TokenReader(tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head
  override def atEnd: Boolean = tokens.isEmpty
  override def pos: Position = NoPosition
  override def rest: Reader[Token] = new TokenReader(tokens.tail)
}

trait BaseParser extends Parsers {
  override type Elem = Token

  def variable: Parser[VARIABLE] = {
    accept("variable", { case newVar @ VARIABLE(_) ⇒ newVar })
  }

  def literal: Parser[Literal] = {
    accept("literal", { case LITERAL(string) ⇒ Literal(string) })
  }

  def simpleIdentifier: Parser[Identifier] = {
    accept("identifier", { case IDENTIFIER(id) ⇒ Identifier(id) })
  }

  def scopedIdentifier: Parser[Identifier] = {
    simpleIdentifier ~ rep1(DOT ~ simpleIdentifier) ^^ {
      case Identifier(first, _) ~ list ⇒
        val result = first :: (list map { case _ ~ Identifier(next, _) ⇒ next })
        Identifier(result.last, result.dropRight(1))
    }
  }

  def identifier: Parser[Identifier] = scopedIdentifier | simpleIdentifier

  def newContainer: Parser[Identifier] = {
    simpleIdentifier ~ LESSTHAN ~ identifier ~ COLON ^^ {
      case Identifier(name, _) ~ _ ~ Identifier(parent, rest) ~ _ ⇒
        Identifier(name, rest :+ parent)
    }
  }

  def openContainer: Parser[Identifier] = {
    log(identifier ~ COLON)("Open container") ^^ {
      case id ~ _ ⇒ id
    }
  }

  def assignContainer: Parser[Identifier] = {
    simpleIdentifier ~ ASSIGN ~ identifier ^^ {
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
