package de.hyronx.matter.compiler.ast

import scala.collection.mutable.ListBuffer

object Types {
  type Definitions = Seq[AST]
  type ContentMap = Map[String, Definitions]
}

trait AST
case class Identifier(name: String, var family: List[String] = List()) extends AST
case class Literal(string: String) extends AST

sealed trait ContentAST extends AST
case class Range(from: Char, to: Char) extends ContentAST
case object Whitespace extends ContentAST
case object CharAny extends ContentAST
case class Regex(definitions: Types.Definitions) extends ContentAST
case class Selection(left: Types.Definitions, right: Types.Definitions) extends ContentAST
case class Repeat(definitions: Types.Definitions) extends ContentAST
case class RepeatOne(definitions: Types.Definitions) extends ContentAST
case class Optional(definitions: Types.Definitions) extends ContentAST
case class Group(definitions: Types.Definitions) extends ContentAST
case class Content(content: Types.ContentMap) extends AST

case class OnCommand(cmd: String, operation: AST) extends AST
case class Lookup(id: Identifier) extends AST
case class LookupList(ids: List[Identifier]) extends AST
case class InDo(id: Identifier, operation: AST) extends AST
