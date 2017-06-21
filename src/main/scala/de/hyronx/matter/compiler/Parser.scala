package de.hyronx.matter.compiler

import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.parsers._

object Parser extends BasicParser {
  import fastparse.all._

  def apply(
    code: String
  ): Either[ParserError, ExpressionList[AST]] = {
    val indent = Indentation(0)
    val typeParser = P(typeDefinition(indent).log() | typeReopening(indent).log()) map (Some(_))
    P((typeParser | comment.map(_ ⇒ None) | multiLineComment.log().map(_ ⇒ None))
      .rep(min = 1, sep = indent.same) ~ nl.? ~ End)
      .map { seq ⇒ ExpressionList(seq.flatten) }
      .parse(code) match {
        case fail: Parsed.Failure      ⇒ Left(ParserError(fail.lastParser.toString))
        case Parsed.Success(result, _) ⇒ Right(result)
      }
  }

  def typeDefinition(indent: Indentation): P[TypeDefinition] = {
    val deeper = Indentation(indent)
    val genericParser = P("[" ~
      expressionList(indent, typeVariable map (TypeVariable(_))) ~
      "]")
    val paramsParser = expressionList(indent, variableName map (Variable(_)))
    val typeDef = ws ~ "<" ~ ws ~ scopedType ~ ":"
    P(
      scopedType ~ genericParser.? ~ paramsParser.? ~ typeDef ~
        expressionList(indent, P(typeDefinition(deeper) | typeReopening(deeper)))
    ) map {
        case (typeName, generic, params, ancestor, body) ⇒ TypeDefinition(
          typeName,
          ancestor,
          generic map (_.ops) getOrElse Seq(),
          params map (_.ops) getOrElse Seq(),
          body.ops
        )
      }
  }

  def typeReopening(indent: Indentation): P[TypeReopening] = {
    val deeper = Indentation(indent)
    val genericParser = P("[" ~ expressionList(indent, scopedType) ~ "]")
    val paramsParser = expressionList(indent, variableName map (Variable(_)))
    val expressions = P(SyntaxParser(deeper).log() | ExpressionsParser(deeper).log() | typeDefinition(deeper) | typeReopening(deeper))
    P(
      scopedType ~ genericParser.? ~ paramsParser.? ~ ":" ~
        expressionList(indent, expressions)
    ) map {
        case (typeName, generic, params, body) ⇒ TypeReopening(
          typeName,
          generic map (_.ops) getOrElse Seq(),
          params map (_.ops) getOrElse Seq(),
          body.ops
        )
      }
  }
}
