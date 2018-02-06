package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.ast
import de.hyronx.matter.compiler.ast.ExpressionList
import de.hyronx.matter.compiler.types.RelativeTypePath
import de.hyronx.matter.library.MutableTree
import fastparse.all

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
    P(typeName.!.rep(min = 1, sep = ".")) map (ast.TypeName(_))
  }

  val variableTypedDecl = P(variableName ~ ws ~ scopedType) map {
    case (varName, typeName) ⇒ ast.Variable(varName, Some(typeName))
  }
  val variableDecl = P(variableName ~ (ws ~ scopedType).?) map {
    case (varName, typeName) ⇒ ast.Variable(varName, typeName)
  }

  def expressionList[T <: ast.AST](
    indent: Indentation,
    possibleExprs: P[T]
  ): P[Stream[T]] = {
    val deeperIndent = indent.deeper

    val oneLiner = P("(".? ~ possibleExprs.rep(min = 1, sep = P("," ~ ws)) ~ ")".?) map (_.toStream)

    val indented = P(deeperIndent ~ (possibleExprs.map(Some(_)) | comment.map(_ ⇒ None))
      .rep(min = 1, sep = deeperIndent)) map {
      case (_, seq: Seq[Option[T]]) ⇒ seq.flatten.toStream
    }

    P(oneLiner | indented)
  }
}
