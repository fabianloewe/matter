package de.hyronx.matter.compiler.parsers

import fastparse.all._
import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.library.MutableTree

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
    case (defs, Some("*")) ⇒ Repeat(defs)
    case (sel: Selection, Some("+")) ⇒ RepeatOne(sel)
    case (defs, Some("+")) ⇒ RepeatOne(defs)
    case (sel: Selection, Some("?")) ⇒ Option(sel)
    case (defs, Some("?")) ⇒ Option(defs)
    case (defs, None) ⇒ defs
  }

  val variableUsage: P[VariableUsage] = variableName map VariableUsage

  val typeParser: P[TypeConstruction] = {
    val genericParser = P("[" ~ expressionList(indent, scopedType) ~ "]")
    val paramsParser = P("(" ~ expressionList(indent, variableName map (Variable(_))) ~ ")")

    P(scopedType ~ genericParser.? ~ paramsParser.?) map {
      case (typeName, generics, params) ⇒ TypeConstruction(
        typeName,
        generics.getOrElse(Seq()),
        params.getOrElse(Seq())
      )
    }
  }

  val definitions: P[AST] = P(group | range | literalAST | typeParser)

  val concatenation: P[AST] /* Concatenation | AST */ = {
    P(definitions | variableUsage).rep(min = 2, sep = ws) map { seq ⇒
      if (seq.lengthCompare(1) > 0)
        AST(Concatenation(), seq.toStream)
      else
        seq.head

    }
  }

  val selection: P[AST] /* Selection | AST */ = {
    P(concatenation | definitions).rep(min = 1, sep = P(ws ~ "|" ~ ws)) map { seq ⇒
      if (seq.lengthCompare(1) > 0)
        AST(Selection(), seq.toStream)
      else
        seq.head
    }
  }

  val body: P[AST] = P(assignExpression(selection) | declareExpression)
}

object SyntaxParser {
  def apply(indent: Indentation): P[AST] = new SyntaxParser(indent).body
}
