package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.ast.Types.Definitions

trait ContentParser extends BaseParser {
  import fastparse.all._

  val range: P[Range] = {
    val rangeLowerCase = P(lowercase.! ~ "-" ~ lowercase.!) map {
      case (from, to) ⇒
        Range(from, to)
    }
    val rangeUpperCase = P(uppercase.! ~ "-" ~ uppercase.!) map {
      case (from, to) ⇒
        Range(from, to)
    }
    val rangeNumber = P(number.! ~ "-" ~ number.!) map {
      case (from, to) ⇒
        Range(from.toString, to.toString)
    }

    P(rangeLowerCase | rangeUpperCase | rangeNumber)
  }

  val end = P(ws | nl)
  val literalAST = P(literal) map (Literal)
  val repeat = P("{" ~/ ws ~ definitions ~ ws ~ "}*") map (Repeat)
  val repeatOne = P("{" ~/ ws ~ definitions ~ ws ~ "}+") map (RepeatOne)
  val optional = P("[" ~/ ws ~ definitions ~ ws ~ "]") map (Optional)
  val group = P("(" ~/ ws ~ definitions ~ ws ~ ")") map (Group)

  private val definitions: P[Definitions] = {
    P(repeat | repeatOne | optional | group |
      range | scopedIdentifier | literalAST).rep(min = 1, sep = ws).log()
  }

  def content(indent: Indentation): P[Content] = {
    P(variable ~ ws.rep(1) ~ "=" ~/ ws ~ definitions).rep(min = 1, sep = indent.same).log() map { result ⇒
      //val result = seq map { case (newVar, defs, _) ⇒ (newVar, defs) }
      Content(result.toMap[String, Definitions])
    }
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
