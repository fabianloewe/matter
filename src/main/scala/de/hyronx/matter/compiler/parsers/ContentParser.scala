package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.tokens._
import de.hyronx.matter.compiler.ast._

trait ContentParser extends BaseParser {
  def range: Parser[Range] = {
    def charLowerCase: Parser[CHAR_LOWER] = {
      accept("lowercase character", { case ch @ CHAR_LOWER(_) ⇒ ch })
    }
    def charUpperCase: Parser[CHAR_UPPER] = {
      accept("uppercase character", { case ch @ CHAR_UPPER(_) ⇒ ch })
    }
    def charNumber: Parser[CHAR_NUM] = {
      accept("numerical character", { case ch @ CHAR_NUM(_) ⇒ ch })
    }

    val rangeLowerCase = charLowerCase ~ DASH ~ charLowerCase ^^ {
      case CHAR_LOWER(from) ~ _ ~ CHAR_LOWER(to) ⇒ Range(from, to)
    }
    val rangeUpperCase = charUpperCase ~ DASH ~ charUpperCase ^^ {
      case CHAR_UPPER(from) ~ _ ~ CHAR_UPPER(to) ⇒ Range(from, to)
    }
    val rangeNumber = charNumber ~ DASH ~ charNumber ^^ {
      case CHAR_NUM(from) ~ _ ~ CHAR_NUM(to) ⇒ Range(from, to)
    }

    rangeLowerCase | rangeUpperCase | rangeNumber
  }

  def repeat: Parser[Repeat] = {
    CURLY_BRACKET_LEFT ~ definitions ~ CURLY_BRACKET_RIGHT ~ ASTERISK ^^ {
      case _ ~ defs ~ _ ~ _ ⇒ Repeat(defs)
    }
  }

  def repeatOne: Parser[RepeatOne] = {
    CURLY_BRACKET_LEFT ~ definitions ~ CURLY_BRACKET_RIGHT ~ PLUS ^^ {
      case _ ~ defs ~ _ ~ _ ⇒ RepeatOne(defs)
    }
  }

  def option: Parser[Option] = {
    SQUARE_BRACKET_LEFT ~ definitions ~ SQUARE_BRACKET_RIGHT ^^ {
      case _ ~ defs ~ _ ⇒ Option(defs)
    }
  }

  def group: Parser[Group] = {
    PARENTHESIS_LEFT ~ definitions ~ PARENTHESIS_RIGHT ^^ {
      case _ ~ defs ~ _ ⇒ Group(defs)
    }
  }

  private def all: Parser[AST] = {
    range | repeat | repeatOne | option | group | literal | identifier
  }

  private def definitions: Parser[Definitions] = {
    rep1(all) ^^ { list ⇒ Definitions(list) }
  }

  def content: Parser[Content] = {
    rep1(
      log(variable ~ ASSIGN ~ definitions)("Content variable") ^^ {
        case VARIABLE(name) ~ _ ~ defs ⇒ (name, defs)
      }
    ) ^^ { list ⇒ Content(list.toMap[String, Definitions]) }
  }

  /*
  def leftParenthesis = "(" ^^ (_ ⇒ PARENTHESIS_LEFT)
  def rightParenthesis = ")" ^^ (_ ⇒ PARENTHESIS_RIGHT)
  def leftCurlyBracket = "{" ^^ (_ ⇒ CURLY_BRACKET_LEFT)
  def rightCurlyBracket = "}" ^^ (_ ⇒ CURLY_BRACKET_RIGHT)
  def leftSquareBracket = "[" ^^ (_ ⇒ SQUARE_BRACKET_LEFT)
  def rightSquareBracket = "]" ^^ (_ ⇒ SQUARE_BRACKET_RIGHT)
  def asterisk = "*" ^^ (_ ⇒ ASTERISK)
  def plus = "+" ^^ (_ ⇒ PLUS)
  */
}
