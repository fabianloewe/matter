package de.hyronx.matter.compiler.ast

case class ExpressionList[+T <: AST](ops: Seq[T]) extends AST

case class Equals(left: AST, right: AST) extends AST

case class If(boolOp: AST, op: AST) extends AST
case class Else(op: AST) extends AST

sealed trait Callee
case class VariableCallee(name: String) extends Callee
case class Call(caller: String, callee: Callee, params: Seq[AST]) extends AST with Callee
case class Assign(variable: Variable, op: AST) extends AST

case class MapType(variable: String, toType: TypeName) extends AST
case class MapCall(variable: String, call: Call) extends AST
