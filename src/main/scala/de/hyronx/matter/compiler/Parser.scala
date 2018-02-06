package de.hyronx.matter.compiler

import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.errors.ParserError
import de.hyronx.matter.compiler.parsers._
import de.hyronx.matter.library.MutableTree

object Parser extends BasicParser {

  import fastparse.all._

  def apply(
    code: String
           ): Either[ParserError, ExpressionList[AST]] = {
    println("Hello?")
    val indent = Indentation(0)
    val typeParser = P(typeDefinition(indent) | typeReopening(indent)) map (Some(_))
    P((typeParser | comment.map(_ ⇒ None) | multiLineComment.map(_ ⇒ None))
      .rep(min = 1, sep = indent.same) ~ nl.? ~ End)
      .map { seq ⇒ AST(ExpressionList[AST](), seq.flatten.toStream) }
      .parse(code) match {
      case Parsed.Failure(lastParser, _, _) ⇒ Left(ParserError(lastParser.toString))
      case Parsed.Success(result, _) ⇒ Right(result)
      }
  }

  def typeDefinition(indent: Indentation): P[TypeDefinition] = {
    val deeper = Indentation(indent)
    val genericParser = P("[" ~
      expressionList(indent, typeVariable map (TypeVariable(_))) ~
      "]")
    val paramsParser = expressionList(indent, variableTypedDecl)
    val typeDef = ws ~ "<" ~ ws ~ scopedType ~ ":"
    P(scopedType ~ genericParser.? ~ paramsParser.? ~ typeDef ~
      expressionList(indent, P(typeDefinition(deeper) | typeReopening(deeper)))) map {
      case (typeName, generic, params, ancestor, body) ⇒ MutableTree[AST, TypeDefinition](
        TypeDefinition(
          typeName,
          ancestor,
          generic.map(_.map { case x: TypeVariable ⇒ x }.toSeq).getOrElse(Seq()),
          params.map(_.map { case x: Variable ⇒ x }.toSeq).getOrElse(Seq())
        ),
        body: _*
      )
    }
  }

  def typeReopening(indent: Indentation): P[TypeReopening] = {
    val deeper = Indentation(indent)
    val genericParser = P("[" ~ expressionList(indent, scopedType) ~ "]")
    val paramsParser = expressionList(indent, variableTypedDecl)
    val expressions = P(SyntaxParser(deeper) | ExpressionsParser(deeper) | typeDefinition(deeper) | typeReopening(deeper))
    P(scopedType ~ genericParser.? ~ paramsParser.? ~ ":" ~
      expressionList(indent, expressions)) map {
      case (typeName, generic, params, body) ⇒ MutableTree(
        TypeReopening(
          typeName,
          generic.map(_.map { case x: TypeName ⇒ x }.toSeq).getOrElse(Seq()),
          params.map(_.map { case x: Variable ⇒ x }.toSeq).getOrElse(Seq())
        ),
        body: _*
      )
    }
  }
}
