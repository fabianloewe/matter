package de.hyronx.matter.compiler.ast

import de.hyronx.matter.compiler.types.TypePath

class ExpressionList[T <: AST]() extends BodyAST
object ExpressionList {
  def apply[T <: AST](ast: Stream[AST] = Stream()): ExpressionList[T] = {
    val exprList = new ExpressionList[T]()
    if (ast.nonEmpty)
      exprList.addChildren(ast: _*)
    exprList
  }
  def unapply[T <: AST](arg: ExpressionList[T]) = Some(arg.children)
}

case class Equals(left: AST, right: AST) extends AST

case class If(boolOp: AST) extends BodyAST

case class Else() extends BodyAST

case class Destruct(variables: Seq[Variable]) extends AST

case class AnonMethodDef(params: Seq[Variable]) extends BodyAST

sealed trait Callee {
  def name: String
}

case class Call(name: String, params: Seq[AST]) extends AST with Callee

case class VariableCallee(name: String) extends Callee

case class ChainedCall(caller: String, callee: Callee) extends AST with Callee {
  val name = caller
}

case class Assign(variable: Variable) extends BodyAST

case class Declare(variable: Variable) extends AST

sealed trait MapAST extends AST {
  def variable: String
}

case class MapType(variable: String, toType: TypePath) extends MapAST

case class MapCall(variable: String, call: ChainedCall) extends MapAST

case class MapMethodDef(variable: String, params: Seq[Variable]) extends MapAST with BodyAST

case class MapExpression(variable: String) extends MapAST with BodyAST

