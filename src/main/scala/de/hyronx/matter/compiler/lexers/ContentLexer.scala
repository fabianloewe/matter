package de.hyronx.matter.compiler.lexers

import de.hyronx.matter.compiler.tokens._

trait ContentLexer extends BaseLexer {
  import fastparse.noApi._
  import whitespace._

  def leftParenthesis = P("(") map (_ ⇒ PARENTHESIS_LEFT)
  def rightParenthesis = P(")") map (_ ⇒ PARENTHESIS_RIGHT)
  def leftCurlyBracket = P("{") map (_ ⇒ CURLY_BRACKET_LEFT)
  def rightCurlyBracket = P("}") map (_ ⇒ CURLY_BRACKET_RIGHT)
  def leftSquareBracket = P("[") map (_ ⇒ SQUARE_BRACKET_LEFT)
  def rightSquareBracket = P("]") map (_ ⇒ SQUARE_BRACKET_RIGHT)

  def asterisk = P("*") map (_ ⇒ ASTERISK)
  def plus = P("+") map (_ ⇒ PLUS)
  def space = P(" ") map (_ ⇒ SPACE)
  def dash = P("-") map (_ ⇒ DASH)

  def content: P[Token] = {
    leftParenthesis | rightParenthesis |
      leftCurlyBracket | rightCurlyBracket |
      leftSquareBracket | rightSquareBracket |
      asterisk | plus
  }
}
