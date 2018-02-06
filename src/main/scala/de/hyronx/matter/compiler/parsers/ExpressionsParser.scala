package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.ast.{ Option ⇒ ASTOption, _ }
import de.hyronx.matter.library.MutableTree
import fastparse.all
import fastparse.all._

class ExpressionsParser(indent: Indentation) extends BasicParser {
  val equalsExpression = P(variableName ~ ws ~ "==" ~ ws ~ variableName) map {
    case (left, right) ⇒ Equals(Variable(left), Variable(right))
  }

  val boolExpression = P(equalsExpression)
  val ifExpression: P[If] = {
    P("if" ~ ws ~ boolExpression ~ ":" ~ ws.? ~ expressionList(indent, all)) map {
      case (boolExpr, body) ⇒ MutableTree(If(boolExpr), body: _*)
    }
  }

  def assignExpression(allowed: P[AST]): P[Assign] = {
    P(variableDecl ~ ws.rep(1) ~ "=" ~ ws ~ allowed) map {
      case (variable, op) ⇒ MutableTree(Assign(variable), op)
    }
  }

  val declareExpression: P[Declare] = variableTypedDecl map Declare

  val callExpression: P[ChainedCall] = {
    val paramList = ("(" ~ expressions.rep(1, sep = ", ") ~ ")") map { expr ⇒
      MutableTree(ExpressionList(), expr: _*)
    }

    P(variableName ~ "." ~
      (callExpression | number | (variableName ~ paramList.?))) map {
      case (caller: String, callee: Call) ⇒ ChainedCall(
        caller,
        callee
      )
      case (caller: String, callee: String) ⇒ ChainedCall(
        caller,
        VariableCallee(callee)
      )
      case (caller: String, (callee: String, exprList: Option[ExpressionList[AST]])) ⇒ ChainedCall(
        caller,
        Call(callee, exprList.map(_.children).getOrElse(Seq()))
      )
    }
  }

  val anonMethodDef: P[AnonMethodDef] = {
    P(expressionList(indent, variableDecl) ~ ":" ~ ws.? ~ expressionList(indent, P(expressions | variableDecl))) map {
      case (params, body) ⇒ MutableTree(AnonMethodDef(params), body: _*)
    }
  }

  val mapExpression: P[AST] = {
    val possibleExprs = P(scopedType | concatExpression | variableDecl | literalAST | callExpression | anonMethodDef)
    P(variableName ~ ws.rep(1) ~ "->" ~ ws ~ possibleExprs) map {
      case (varName: String, toType: TypeName)                     ⇒ AST(MapType(varName, toType))
      case (varName: String, call: ChainedCall)                    ⇒ AST(MapCall(varName, call))
      case (varName: String, BodyAST(AnonMethodDef(params), body)) ⇒ AST(MapMethodDef(varName, params), body)
      case (varName: String, ast)                                  ⇒ AST(MapExpression(varName), Stream(ast))
    }
  }

  val concatExpression: P[Concatenation] = {
    P((callExpression | variableDecl | literalAST).rep(min = 2, sep = ws)) map { seq ⇒
      AST(Concatenation(), seq.toStream)
    }
  }

  val literalAST: P[Literal] = literal map Literal

  val expressions = P(ifExpression | mapExpression | concatExpression | callExpression | literalAST)
  val all = P(assignExpression(expressions) | expressions)
}

object ExpressionsParser {
  def apply(indent: Indentation) = new ExpressionsParser(indent).all
}
