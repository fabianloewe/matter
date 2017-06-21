package de.hyronx.matter.compiler.parsers

import fastparse.all._

import de.hyronx.matter.compiler.ast._

class ExpressionsParser(indent: Indentation) extends BasicParser {
  val equalsExpression = P(variableName ~ ws ~ "==" ~ ws ~ variableName) map {
    case (left, right) ⇒ Equals(Variable(left), Variable(right))
  }

  val boolExpression = P(equalsExpression)
  val ifExpression: P[If] = {
    P("if" ~ ws ~ boolExpression ~ ":" ~ expressionList(indent, all)) map {
      case (boolExpr, body) ⇒ If(boolExpr, body)
    }
  }

  def assignExpression(allowed: P[AST]) = {
    P(variableDecl ~ ws.rep(1) ~ "=" ~ ws ~ allowed) map {
      case (variable, op) ⇒ Assign(variable, op)
    }
  }

  val callExpression: P[Call] = {
    P(variableName ~ "." ~ (callExpression | variableName | number) ~ ws.? ~ expressionList(indent, expressions).?).log() map {
      case (caller: String, callee: Call, exprList) ⇒ Call(
        caller,
        callee,
        exprList map (_.ops) getOrElse Seq()
      )
      case (caller: String, callee, exprList) ⇒ Call(
        caller,
        VariableCallee(callee.toString),
        exprList map (_.ops) getOrElse Seq()
      )
    }
  }

  val mapExpression: P[AST] = {
    P(variableName ~ ws.rep(1) ~ "->" ~ ws ~ (scopedType | callExpression)) map {
      case (varName: String, toType: TypeName) ⇒ MapType(varName, toType)
      case (varName: String, call: Call)       ⇒ MapCall(varName, call)
    }
  }

  val literalAST: P[Literal] = literal map (Literal)

  val expressions = P(ifExpression | mapExpression | callExpression | literalAST)
  val all = P(expressions | assignExpression(expressions))
}

object ExpressionsParser {
  def apply(indent: Indentation) = new ExpressionsParser(indent).all
}
