package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.ast.AST.Definitions

class SyntaxParser(indent: Indentation) extends BaseParser {
  import fastparse.all._

  val range: P[Range] = {
    val rangeLowerCase = P("\"" ~ lowercase.! ~ "\"..\"" ~ lowercase.! ~ "\"") map {
      case (from, to) ⇒ Range(from.head, to.head)
    }
    val rangeUpperCase = P("\"" ~ uppercase.! ~ "\"..\"" ~ uppercase.! ~ "\"") map {
      case (from, to) ⇒ Range(from.head, to.head)
    }
    val rangeNumber = P("\"" ~ number.! ~ "\"..\"" ~ number.! ~ "\"") map {
      case (from, to) ⇒ Range(from.toString.head, to.toString.head)
    }

    P(rangeLowerCase | rangeUpperCase | rangeNumber)
  }

  val end = P(ws | nl)
  val literalAST = literal map (Literal)
  val repeat = P("{" ~ ws ~ selection ~ ws ~ "}*") map {
    case sel: Selection ⇒ Repeat(sel)
    case defs           ⇒ Repeat(defs)
  }

  val repeatOne = P("{" ~ ws ~ selection ~ ws ~ "}+") map {
    case sel: Selection ⇒ RepeatOne(sel)
    case defs           ⇒ RepeatOne(defs)
  }

  val option = P("[" ~ ws ~ selection ~ ws ~ "]") map {
    case sel: Selection ⇒ Option(sel)
    case defs           ⇒ Option(defs)
  }

  val group = P("(" ~ ws ~ selection ~ ws ~ ")")

  val concatenation: P[AST] = {
    P(repeat | repeatOne | option | group | range | scopedType |
      variableName.map(VariableUsage) |
      literalAST)
      .rep(min = 1, sep = ws) map { seq ⇒
        if (seq.size > 1)
          Concatenation(seq)
        else
          seq(0)
      }
  }

  val selection: P[AST] = concatenation.rep(min = 1, sep = P(ws ~ "|" ~ ws)) map { seq ⇒
    if (seq.size > 1)
      Selection(seq)
    else
      seq(0)
  }

  val syntax: P[AST.SyntaxMap] = {
    P(
      // Check if there is a variable definition
      (variableName ~ ws.rep(1) ~ "=" ~/ ws ~ selection) |
        // Check if there is a variable declaration
        variableDecl.map {
          // It is so try to create a Tuple2 to later build a map from
          case VariableDecl(varName, varType) ⇒ (
            varName,
            Declaration(varType.name match {
              case "String"  ⇒ SyntaxType.String
              case "Grammar" ⇒ SyntaxType.Grammar
            })
          )
        }
    ).rep(min = 1, sep = indent.same) map { result ⇒
        println(s"` Content map: $result")
        // TODO: Add check if all variables exist
        collection.mutable.Map(result: _*)
      }
  }
}

object SyntaxParser {
  def apply(indent: Indentation) = new SyntaxParser(indent).syntax
}
