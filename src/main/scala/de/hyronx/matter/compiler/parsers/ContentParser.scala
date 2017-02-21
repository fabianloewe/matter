package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.ast.Types.Definitions

trait ContentParser extends BaseParser {
  import fastparse.all._

  val range: P[Range] = {
    val rangeLowerCase = P(lowercase.! ~ "-" ~ lowercase.!) map {
      case (from, to) ⇒ Range(from.head, to.head)
    }
    val rangeUpperCase = P(uppercase.! ~ "-" ~ uppercase.!) map {
      case (from, to) ⇒ Range(from.head, to.head)
    }
    val rangeNumber = P(number.! ~ "-" ~ number.!) map {
      case (from, to) ⇒ Range(from.toString.head, to.toString.head)
    }

    P(rangeLowerCase | rangeUpperCase | rangeNumber).log()
  }

  val end = P(ws | nl)
  val literalAST = P(literal) map (Literal)
  val regex = P(range.rep(1)) map (Regex)
  val repeat = P("{" ~/ ws ~ selection ~ ws ~ "}*") map (Repeat)
  val repeatOne = P("{" ~/ ws ~ selection ~ ws ~ "}+") map (RepeatOne)
  val optional = P("[" ~/ ws ~ selection ~ ws ~ "]") map (Optional)
  val group = P("(" ~/ ws ~ selection ~ ws ~ ")") map (Group)

  val definitions: P[Definitions] = {
    P(repeat | repeatOne | optional | group | regex | scopedIdentifier |
      literalAST)
      .rep(min = 1, sep = ws).log()
  }

  val selection = P(definitions ~ (ws ~ "|" ~/ ws ~ definitions).?) map (_ match {
    case (left, Some(right)) ⇒ List(Selection(left, right))
    case (left, None)        ⇒ left
  })

  def content(indent: Indentation): P[Content] = {
    val contentWithVars = {
      P(variable ~ ws.rep(1) ~ "=" ~/ ws ~ selection)
        .rep(min = 1, sep = indent.same).log() map { result ⇒
          Content(result.toMap[String, Definitions])
        }
    }
    //val contentWithout = P(definitions) map { defs ⇒ Content(Map("" → defs)) }
    contentWithVars
  }

  /*
  def leftParenthesis = "(" map (_ ⇒ PARENTHESIS_LEFT)
  def rightParenthesis = ")" map (_ ⇒ PARENTHESIS_RIGHT)
  def leftCurlyBracket = "{" map (_ ⇒ CURLY_BRACKET_LEFT)
  def rightCurlyBracket = "}" map (_ ⇒ CURLY_BRACKET_RIGHT)
  def leftSquareBracket = "[" map (_ ⇒ SQUARE_BRACKET_LEFT)
  def rightSquareBracket = "]" map (_ ⇒ SQUARE_BRACKET_RIGHT)
  def asterisk = "*" map (_ ⇒ ASTERISK)
  def plus = "+" map (_ ⇒ PLUS)
  */
}
