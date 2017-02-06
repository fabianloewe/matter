package de.hyronx.matter.compiler.lexers

import scala.util.parsing.combinator.RegexParsers

import de.hyronx.matter.compiler.tokens._

trait BaseLexer extends RegexParsers {
  override def skipWhitespace = true
  override val whiteSpace = "[ \t\r\f]+".r

  def variable: Parser[VARIABLE] = {
    """[a-z][a-z0-9\-]*""".r ^^ { string ⇒ VARIABLE(string) }
  }

  def identifier: Parser[IDENTIFIER] = {
    """[A-Z][a-zA-Z0-9\-]*""".r ^^ { string ⇒ IDENTIFIER(string) }
  }

  def literal: Parser[LITERAL] = {
    """"[^"]+"""".r ^^ { str ⇒
      val content = str.substring(1, str.length - 1)
      LITERAL(content)
    }
  }

  def indentation: Parser[INDENTATION] = {
    "\n[ ]*".r ^^ { whitespace ⇒
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  def endline = "\n" ^^ (_ ⇒ ENDLINE)
  def colon = ":" ^^ (_ ⇒ COLON)
  def dot = "." ^^ (_ ⇒ DOT)
  def lessThan = "<" ^^ (_ ⇒ LESSTHAN)
  def assign = "=" ^^ (_ ⇒ ASSIGN)

  def base: Parser[Token] = {
    colon | dot | lessThan | assign | endline |
      variable | identifier | literal | indentation
  }

  protected def processIndentation(
    tokens: List[Token],
    indents: List[Int] = List(0)
  ): List[Token] = tokens.headOption match {
    case Some(INDENTATION(spaces)) if spaces > indents.head ⇒
      INDENT :: processIndentation(tokens.tail, spaces :: indents)

    case Some(INDENTATION(spaces)) if spaces < indents.head ⇒
      val (dropped, kept) = indents.partition(_ > spaces)
      (dropped map (_ ⇒ DEDENT)) ::: processIndentation(tokens.tail, kept)

    case Some(INDENTATION(spaces)) if spaces == indents.head ⇒
      processIndentation(tokens.tail, indents)

    case Some(token) ⇒ token :: processIndentation(tokens.tail, indents)

    case None        ⇒ indents.filter(_ > 0).map(_ ⇒ DEDENT)
  }
}
