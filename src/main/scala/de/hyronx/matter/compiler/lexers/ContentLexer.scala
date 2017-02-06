package de.hyronx.matter.compiler.lexers

import de.hyronx.matter.compiler.tokens._

trait ContentLexer extends BaseLexer {
  def leftParenthesis = "(" ^^ (_ ⇒ PARENTHESIS_LEFT)
  def rightParenthesis = ")" ^^ (_ ⇒ PARENTHESIS_RIGHT)
  def leftCurlyBracket = "{" ^^ (_ ⇒ CURLY_BRACKET_LEFT)
  def rightCurlyBracket = "}" ^^ (_ ⇒ CURLY_BRACKET_RIGHT)
  def leftSquareBracket = "[" ^^ (_ ⇒ SQUARE_BRACKET_LEFT)
  def rightSquareBracket = "]" ^^ (_ ⇒ SQUARE_BRACKET_RIGHT)
  def asterisk = "*" ^^ (_ ⇒ ASTERISK)
  def plus = "+" ^^ (_ ⇒ PLUS)
  def space = " " ^^ (_ ⇒ SPACE)
  def dash = "-" ^^ (_ ⇒ DASH)

  def char: Parser[Token] = {
    val charLowerCase = "[a-z]".r ^^ { ch ⇒ CHAR_LOWER(ch) }
    val charUpperCase = "[A-Z]".r ^^ { ch ⇒ CHAR_UPPER(ch) }
    val charNumber = "[0-9]".r ^^ { ch ⇒ CHAR_NUM(ch) }
    charLowerCase | charUpperCase | charNumber
  }

  def content: Parser[Token] = {
    leftParenthesis | rightParenthesis |
      leftCurlyBracket | rightCurlyBracket |
      leftSquareBracket | rightSquareBracket |
      asterisk | plus
  }
}
