package de.hyronx.matter.compiler.ast

import scala.collection.mutable.ListBuffer

trait AST
case class Identifier(name: String, var family: List[String] = List()) extends AST
case class Literal(string: String) extends AST

sealed trait ContentAST extends AST
case class Definitions(list: List[AST]) extends ContentAST
case class Range(from: String, to: String) extends ContentAST
case class Repeat(definitions: Definitions) extends ContentAST
case class RepeatOne(definitions: Definitions) extends ContentAST
case class Option(definitions: Definitions) extends ContentAST
case class Group(definitions: Definitions) extends ContentAST
case class Content(content: Map[String, ContentAST]) extends ContentAST

case class OnCommand(cmd: String, operation: AST) extends AST
case class Lookup(id: Identifier) extends AST
case class LookupList(ids: List[Identifier]) extends AST
case class InDo(id: Identifier, operation: AST) extends AST
