package de.hyronx.matter.compiler.parsers

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.ast

trait BaseParser {
  import Indentation._
  import fastparse.all._

  def deeper(indent: Option[Indentation], parent: ast.MatterTypeTree) = {
    indent.getOrElse(Indentation(parent)).deeper map { x ⇒
      println(s"Deeper indent: ${x.indent}")
      x
    }
  }

  def same(indent: Option[Indentation], parent: ast.MatterTypeTree) = {
    indent.getOrElse(Indentation(parent)).same map { x ⇒
      println(s"Same indent: ${x.indent}")
      x
    }
  }

  val ws = Indentation.ws
  val nl = Indentation.nl

  val letter = P(lowercase | uppercase)
  val lowercase = P(CharIn('a' to 'z'))
  val uppercase = P(CharIn('A' to 'Z'))
  val digit = P(CharIn('0' to '9'))

  val variableName = P(lowercase ~ (lowercase | digit | "-").rep).!
  val typeName = P(uppercase ~ (letter | digit | "-").rep).!
  val literal = P("\"" ~ CharsWhile(_ != '\"').! ~ "\"")
  val number = P(digit.rep(1)).! // map (_.toInt)

  val scopedType: P[ast.TypeName] = {
    P(typeName.!.rep(min = 1, sep = ".")) map { seq ⇒
      ast.TypeName(seq.last, seq.dropRight(1).toList)
    }
  }

  val paramType: P[ast.ParamTypeName] = {
    P(scopedType ~ "[" ~ scopedType ~ "]") map {
      case (typeName, param) ⇒ ast.ParamTypeName(typeName, param)
    }
  }

  val typeDecl: P[ast.AST] = scopedType | paramType

  val variableDecl = variableName ~ ws ~ scopedType map {
    case (varName, typeId) ⇒ ast.VariableDecl(varName, typeId)
  }
}
