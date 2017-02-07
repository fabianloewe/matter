package de.hyronx.matter.compiler.lexers

import de.hyronx.matter.compiler.tokens._

trait BaseLexer {
  val whitespace = fastparse.WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep(1))
  }
  import fastparse.noApi._
  import whitespace._

  val letter = P(lowercase | uppercase)
  val lowercase = P(CharIn('a' to 'z'))
  val uppercase = P(CharIn('A' to 'Z'))
  val digit = P(CharIn('0' to '9'))

  val variable: P[VARIABLE] = P(lowercase ~ (lowercase | digit | "-").rep).!.map(VARIABLE)
  val identifier: P[IDENTIFIER] = P(uppercase ~ (letter | digit | "-").rep).!.map(IDENTIFIER)
  val literal: P[LITERAL] = P("\"" ~ CharsWhile(_ != "\"")).!.map(LITERAL)

  val indentation: P[INDENTATION] = P("\n" ~ " ".rep.!).map { whitespaces ⇒
    INDENTATION(whitespaces.length)
  }

  val colon = P(":") map (_ ⇒ COLON)
  val dot = P(".") map (_ ⇒ DOT)
  val lessThan = P("<") map (_ ⇒ LESSTHAN)
  val assign = P("=") map (_ ⇒ ASSIGN)

  val base: P[Token] = {
    colon | dot | lessThan | assign |
      variable | identifier | literal | indentation
  }

  protected def processIndentation(
    tokens: Seq[Token],
    indents: List[Int] = List(0)
  ): List[Token] = {
    tokens.headOption match {
      case Some(INDENTATION(spaces)) if spaces > indents.head ⇒
        ENDLINE :: INDENT :: processIndentation(tokens.tail, spaces :: indents)

      case Some(INDENTATION(spaces)) if spaces < indents.head ⇒
        val (dropped, kept) = indents.partition(_ > spaces)
        ENDLINE :: (dropped map (_ ⇒ DEDENT)) ::: processIndentation(tokens.tail, kept)

      case Some(INDENTATION(spaces)) if spaces == indents.head ⇒
        ENDLINE :: processIndentation(tokens.tail, indents)

      case Some(token) ⇒ token :: processIndentation(tokens.tail, indents)

      case None        ⇒ indents.filter(_ > 0).map(_ ⇒ DEDENT)
    }
  }
}
