package de.hyronx.matter.compiler.parsers

import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.types._

class MappingParser(syntaxVars: AST.SyntaxMap, indent: Indentation) extends BaseParser {
  case class SyntaxVariable(varType: Type, defs: AST)

  println("New instance")

  import fastparse.all._

  val variables = syntaxVars.collect {
    case (varName, defs) ⇒ (varName, getType(defs))
  }.toMap[String, SyntaxVariable]

  private def getType(ast: AST): SyntaxVariable = {
    def matchSyntax(ast: AST): Type = ast match {
      case Option(internalDefs)    ⇒ OptionalType(getType(internalDefs).varType)
      case Repeat(internalDefs)    ⇒ ListType(getType(internalDefs).varType)
      case RepeatOne(internalDefs) ⇒ ListType(getType(internalDefs).varType)
      case Literal(string)         ⇒ StringType
      // TODO: Add real implementation
      case _                       ⇒ VoidType
    }

    def matchTupleTypes(ast: Seq[AST]): List[Type] = ast.headOption match {
      case Some(head) ⇒ matchSyntax(head) :: matchTupleTypes(ast.tail)
      case None       ⇒ List()
    }

    SyntaxVariable(
      ast match {
        case Concatenation(defs) ⇒ TupleType(matchTupleTypes(defs))
        case Declaration(syntaxType) ⇒ syntaxType match {
          case SyntaxType.String  ⇒ StringType
          case SyntaxType.Grammar ⇒ TupleType
        }
        case ast ⇒ matchSyntax(ast)
      },
      ast
    )
  }

  val callExpression: P[CallExpression] = {
    P(variableName ~ "." ~ (variableName | number)) map {
      case (varName, methodName) ⇒ variables get varName match {
        case Some(syntaxType) ⇒
          syntaxType.varType findMethod methodName match {
            case Some(method) ⇒ Some(CallExpression(Variable(varName, syntaxType.varType), method))
            case None         ⇒ None
          }
        case None ⇒ None
      }
    } filter (_.isDefined) map (_.get)
  }

  val expression = callExpression

  val boolExpression: P[CallExpression] = {
    callExpression map { call: CallExpression ⇒
      if (call.method.retType == BoolType)
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

  /*
  val operation = {
    Pass
  }

  val mapStatement = {
    P(variable ~ " -> " ~ typeName) flatMap {
      case (syntaxVar, _, id) ⇒ operation.rep map { ops ⇒
        Mapping(syntaxVar, id, ops)
      }
    }
  }
  */

  private val mapFactory: PartialFunction[(String, String, TypeName, scala.Option[Type]), Mapping] = {
    // typeName was found
    case (syntaxVar, mappedVar, typeName, typeDef) ⇒ MappingExpression(
      syntaxVar,
      mappedVar,
      typeDef.get,
      typeName.name match {
        case "String" ⇒ StringMapping
        case "Int"    ⇒ IntMapping
        // TODO: Implement
        case _        ⇒ StringMapping
      }
    )
  }

  private val mapParser = P(variableName ~ " -> " ~ (scopedType | variableDecl))

  val mapStatement: P[Mapping] = {
    P(mapParser ~ ":").map {
      // Check if typeName is a valid type
      case (syntaxVar, typeName: TypeName) ⇒
        (syntaxVar, syntaxVar, Type(typeName))
      case (syntaxVar, VariableDecl(varName, varType)) ⇒
        (syntaxVar, varName, Type(varType))
      case (_, _) ⇒
        // This is acceptable because filter will fail on None (3th element)
        // while this approach keeps the types correct
        ("", "", None)
    } filter (_._3.isDefined) flatMap {
      case (syntaxVar, mappedVar, mappedType) ⇒
        body map { seq ⇒
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
