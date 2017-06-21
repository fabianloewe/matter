package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.ast

trait BasicParser {
  import fastparse.all._

  val ws = Indentation.ws
  val nl = Indentation.nl

  val letter = P(lowercase | uppercase)
  val lowercase = P(CharIn('a' to 'z'))
  val uppercase = P(CharIn('A' to 'Z'))
  val digit = P(CharIn('0' to '9'))

  val variableName = P((lowercase ~ (lowercase | digit | "-").rep) | "_" | ("$" ~ digit)).!
  val typeName = P(uppercase ~ (letter | digit | "-").rep).!
  val typeVariable = P(uppercase ~ digit.?).!
  val literal = P("\"" ~ CharsWhile(_ != '\"').! ~ "\"")
  val number = P(digit.rep(1)).! // map (_.toInt)
  val comment = P((nl | ws.rep).? ~ "//" ~ CharsWhile(_ != '\n'))
  val multiLineComment = P((nl | ws.rep).? ~ "/*" ~ CharsWhile(_ != '*') ~ "/")

  val scopedType: P[ast.TypeName] = {
    P(typeName.!.rep(min = 1, sep = ".")) map { seq ⇒
      ast.TypeName(seq.last, seq.dropRight(1).toList)
    }
  }

  val variableTypedDecl = P(variableName ~ ws ~ typeName) map {
    case (varName, typeName) ⇒ ast.Variable(varName, Some(typeName))
  }
  val variableDecl = P(variableName ~ (ws ~ typeName).?) map {
    case (varName, typeName) ⇒ ast.Variable(varName, typeName)
  }

  def expressionList[T <: ast.AST](
    indent: Indentation,
    possibleExprs: P[T]
  ): P[ast.ExpressionList[T]] = {
    val deeperIndent = indent.deeper

    val oneLiner = P("(".!.? ~ possibleExprs.rep(min = 1, sep = P(ws ~ ",")) ~ ")".!.?) map {
      case (Some(_), exprs, Some(_)) ⇒ Some(exprs)
      case (None, exprs, None)       ⇒ Some(exprs)
      case _                         ⇒ None
    } filter (_.isDefined) map (_.get) map (ast.ExpressionList(_))

    val indented = P(deeperIndent ~ (possibleExprs.map(Some(_)) | comment.map(_ ⇒ None))
      .rep(min = 1, sep = deeperIndent).log()) map {
      case (_, seq: Seq[Option[T]]) ⇒ ast.ExpressionList(seq.flatten)
    }

    P(oneLiner | indented)
  }
}
