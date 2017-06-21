package de.hyronx.matter.compiler.parsers

import fastparse.all._

import de.hyronx.matter.compiler.ast._

class SyntaxParser(indent: Indentation) extends ExpressionsParser(indent) {
  val range: P[Range] = {
    val rangeLowerCase = P("\"" ~ lowercase.! ~ "\"..\"" ~ lowercase.! ~ "\"") map {
      case (from, to) ⇒ Range(from, to)
    }
    val rangeUpperCase = P("\"" ~ uppercase.! ~ "\"..\"" ~ uppercase.! ~ "\"") map {
      case (from, to) ⇒ Range(from, to)
    }
    val rangeNumber = P("\"" ~ number.! ~ "\"..\"" ~ number.! ~ "\"") map {
      case (from, to) ⇒ Range(from.toString, to.toString)
    }

    P(rangeLowerCase | rangeUpperCase | rangeNumber)
  }

  val group: P[AST] = P("(" ~ selection ~ ")" ~ ("*".! | "+".! | "?".!).?) map {
    case (sel: Selection, Some("*")) ⇒ Repeat(sel)
    case (defs, Some("*"))           ⇒ Repeat(defs)
    case (sel: Selection, Some("+")) ⇒ RepeatOne(sel)
    case (defs, Some("+"))           ⇒ RepeatOne(defs)
    case (sel: Selection, Some("?")) ⇒ Option(sel)
    case (defs, Some("?"))           ⇒ Option(defs)
    case (defs, None)                ⇒ defs
  }

  val variableUsage = variableName map (VariableUsage(_))

  val typeParser = {
    val genericParser = P("[" ~ expressionList(indent, scopedType) ~ "]")
    val paramsParser = P("(" ~ expressionList(indent, variableName map (Variable(_))) ~ ")")

    P(scopedType ~ genericParser.? ~ paramsParser.?) map {
      case (typeName, generics, params) ⇒ TypeConstruction(
        typeName,
        generics map (_.ops) getOrElse Seq(),
        params map (_.ops) getOrElse Seq()
      )
    }
  }

  val definitions = P(group | range | literalAST | typeParser)

  val concatenation = {
    P(definitions | variableUsage).rep(min = 2, sep = ws) map { seq ⇒
      if (seq.size > 1)
        Concatenation(seq)
      else
        seq(0)

    }
  }

  val selection = {
    P(concatenation | definitions).rep(min = 1, sep = P(ws ~ "|" ~ ws)) map { seq ⇒
      if (seq.size > 1)
        Selection(seq)
      else
        seq(0)
    }
  }

  val body = P(assignExpression(selection).log() | variableTypedDecl.log())
}

object SyntaxParser {
  def apply(indent: Indentation) = new SyntaxParser(indent).body
}
