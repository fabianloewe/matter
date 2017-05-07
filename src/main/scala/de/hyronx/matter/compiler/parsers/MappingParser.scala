package de.hyronx.matter.compiler.parsers

import java.util.NoSuchElementException

import de.hyronx.matter.compiler.ParserError
import de.hyronx.matter.compiler.Helpers._
import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.types._

class MappingParser(syntaxVars: AST.SyntaxMap, indent: Indentation) extends BaseParser {
  case class SyntaxVariable(varType: Type, defs: AST)

  import fastparse.all._

  val variables = syntaxVars.collect {
    case (varName, defs) ⇒ varName → SyntaxVariable(defs.getType(syntaxVars), defs)
  }.toMap[String, SyntaxVariable]

  println(s"MappingParser:variables! Map: $variables")

  val callExpression: P[CallExpression] = {
    // Chained calls must know the caller's type
    def chainedCallExpression(varType: Type): P[CallExpression] = {
      // Caller may have a number as a member (e.g. for Collection types)
      P((number | variableName) ~ ".") flatMap { varName ⇒
        // Check if the caller ('varType') contains this member ('varName')
        varType findMember varName match {
          case Some(callerVar) ⇒
            P(chainedCallExpression(callerVar.varType) | number | variableName) map {
              case call: CallExpression ⇒ Some(CallExpression(callerVar, call))
              case callee: String ⇒ callerVar.varType findMember callee match {
                case Some(target) ⇒ Some(CallExpression(callerVar, target))
                case None         ⇒ None
              }
            } filter (_.isDefined) map (_.get)
          case None ⇒ Fail
        }
      }
    }

    // The first call must be made by a known variable
    P(variableName ~ ".") flatMap { varName ⇒
      // Find the specified variable ('varName')
      variables get varName match {
        case Some(callerVar) ⇒
          P(chainedCallExpression(callerVar.varType) | number | variableName) map {
            case call: CallExpression ⇒
              Some(CallExpression(Variable(varName, callerVar.varType), call))
            case callee: String ⇒ callerVar.varType findMember callee match {
              case Some(target) ⇒ Some(CallExpression(Variable(varName, callerVar.varType), target))
              case None         ⇒ None
            }
          } filter (_.isDefined) map (_.get)
        case None ⇒ Fail
      }
    }
  }

  val expression = callExpression

  val boolExpression: P[CallExpression] = {
    callExpression map { call: CallExpression ⇒
      if (call.varType == BoolType)
        call
      else
        None
    } flatMap {
      case None ⇒ Fail
    }
  }

  val ifStatement = {
    P("if " ~ boolExpression ~ ":" ~ body) map {
      case (boolOp, ops) ⇒ IfStatement(boolOp, ops)
    }
  }

  val statement = ifStatement

  val operations: P[Seq[MappingAST]] = {
    (statement | expression).rep(min = 1, sep = indent.same)
  }

  val body = {
    P(indent.deeper ~
      new MappingParser(syntaxVars, Indentation(indent)).operations ~
      indent.same.?) map {
      case (_, ops, _) ⇒ ops
    }
  }

  private val mapFactory: PartialFunction[(String, String, TypeName, scala.Option[Type]), Mapping] = {
    // Called if typeName was found
    case (syntaxVar, mappedVar, typeName, typeDef) ⇒ MappingExpression(
      syntaxVar,
      mappedVar,
      typeDef.get,
      typeName.name match {
        case "String" ⇒ StringMapping
        case "Bool"   ⇒ BoolMapping
        case "Int"    ⇒ IntMapping
        case "Float"  ⇒ FloatMapping
      }
    )
  }

  private val mapParser = P(variableName ~ " -> " ~ (scopedType | variableDecl))

  val mapStatement: P[Mapping] = {
    P(mapParser ~ ":") map {
      // Check if typeName is a valid type
      case (syntaxVar, typeName: TypeName) ⇒
        (syntaxVar, syntaxVar, Type(typeName))
      case (syntaxVar, VariableDecl(varName, varType)) ⇒
        (syntaxVar, varName, Type(varType))
      case (_, _) ⇒
        // This is acceptable because filter will fail on None (3rd element)
        // while this approach keeps the types correct
        ("", "", None)
    } filter (_._3.isDefined) flatMap {
      case (syntaxVar, mappedVar, mappedType) ⇒ body map { seq ⇒
        MappingStatement(syntaxVar, mappedVar, mappedType.get, seq)
      }
    }
  }

  val mapExpression: P[Mapping] = {
    mapParser map {
      // Check if typeName is a valid type
      case (syntaxVar, typeName: TypeName) ⇒
        (syntaxVar, syntaxVar, typeName, Type(typeName))
      case (syntaxVar, VariableDecl(varName, varType)) ⇒
        (syntaxVar, varName, varType, Type(varType))
      case (_, _) ⇒
        // See mapStatement
        ("", "", TypeName("", List()), None)
    } filter (_._4.isDefined) map mapFactory
  }

  val mapping: P[Mappings] = {
    (mapStatement | mapExpression).rep(1) map { seq ⇒
      Mappings(seq)
    }
  }
}

object MappingParser {
  def apply(
    variables: AST.SyntaxMap,
    indent: Indentation
  ) = new MappingParser(variables, indent).mapping
}
