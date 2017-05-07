package de.hyronx.matter.compiler.generators

import scala.collection.mutable.Map

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._

import de.hyronx.matter.library._

import de.hyronx.matter.Config
import de.hyronx.matter.compiler.{ Generator, GeneratorError }
import de.hyronx.matter.compiler.Helpers._
import de.hyronx.matter.compiler.types._
import de.hyronx.matter.compiler.ast._

class MappingGenerator(
    matterType: MatterType,
    pbClass: ClassFile
)(
    implicit
    config: Config,
    pkg: Package
) {
  private def generateTypeSpecificCall(
    varType: Type,
    callee: VariableLike,
    codeHandler: CodeHandler
  ): Unit = (varType, callee) match {
    case (TupleType(wrappedTypes), Variable(name, varType)) ⇒ codeHandler <<
      Comment(s"Tuple calls member ($name)") <<
      Ldc(name.toInt) <<
      InvokeInterface("scala/collection/Seq", "apply", s"(I)$OBJECT_TYPE")
    case (TupleType(wrappedTypes), CallExpression(nextCaller, nextCallee)) ⇒ nextCaller match {
      case Variable(nextName, nextType) ⇒
        codeHandler <<
          Comment(s"Tuple calls member (${nextCaller.name} ${nextCaller.varType}) which calls something (${nextCallee.name} ${nextCallee.varType})") <<
          Ldc(nextName.toInt) <<
          InvokeInterface("scala/collection/Seq", "apply", s"(I)$OBJECT_TYPE")
        generateTypeSpecificCall(nextType, nextCallee, codeHandler)
    }
    case (StructType(thatMatterType), Variable(name, varType)) ⇒
      if (thatMatterType.mappings exists (_.mappedVar == name)) {
        codeHandler <<
          Comment(s"Struct (${thatMatterType.id}) calls member ($name $varType)") <<
          // WARN: Possible error because of GetField twice with generateCall()
          CheckCast(thatMatterType.toJavaClass) <<
          GetField(thatMatterType.toJavaClass, name, callee.varType.toJavaType)
      }
    case x ⇒ throw new GeneratorError(s"Not implemented yet: $x")
  }

  private def generateCall(
    caller: VariableLike,
    callee: VariableLike,
    codeHandler: CodeHandler
  ) = caller match {
    case Variable(name, varType) ⇒
      codeHandler <<
        Comment(s"Variable: $name, Type: ${varType.name}") <<
        ArgLoad(1)
      //CheckCast(varType.toJavaClass)
      generateTypeSpecificCall(varType, callee, codeHandler)
  }

  private def generateStringMapping(
    mappedVar: String,
    syntaxAST: AST,
    codeHandler: CodeHandler
  ): Unit = {
    def isConvertible(ast: AST): Boolean = ast match {
      case Enclosing(defs)          ⇒ isConvertible(defs)
      case EnclosingMultiple(defs)  ⇒ defs map (isConvertible(_)) forall (_ == true)
      case Literal(_) | Range(_, _) ⇒ true

      case VariableUsage(varName) ⇒ matterType.syntax find { case (syntaxVar, _) ⇒ syntaxVar == varName } match {
        case Some((_, defs)) ⇒ isConvertible(defs)
        case None            ⇒ println(s"ParserGenerator:isConvertibleToString! Variable is nowhere defined: $varName"); false
      }
      case _: TypeName ⇒ false
    }

    if (isConvertible(syntaxAST)) {
      codeHandler <<
        ArgLoad(1) <<
        InvokeStatic(WRAPPER_CLASS, "mkString", s"($OBJECT_TYPE)$STRING_TYPE")
    } else {
      throw new GeneratorError("Variable cannot automatically be mapped to String")
    }
  }

  private def generateFloatMapping(
    mappedVar: String,
    syntaxAST: AST,
    codeHandler: CodeHandler
  ): Unit = {
    var pointFound = false
    def isConvertible(ast: AST): Boolean = ast match {
      case Enclosing(defs)         ⇒ isConvertible(defs)
      case EnclosingMultiple(defs) ⇒ defs map (isConvertible(_)) forall (_ == true)
      case Literal(string) ⇒
        if ((string contains '.') && pointFound == false) {
          pointFound = true
          true
        } else if ((string contains '.') && pointFound == true) {
          false
        } else {
          string matches "[0-9]+"
        }
      case Range(from, to) ⇒ (from matches "[0-9]+") && (to matches "[0-9]+")
      case VariableUsage(varName) ⇒ matterType.syntax find { case (syntaxVar, _) ⇒ syntaxVar == varName } match {
        case Some((_, defs)) ⇒ isConvertible(defs)
        case None            ⇒ println(s"ParserGenerator:isConvertibleToFloat! Variable is nowhere defined: $varName"); false
      }
      case _: TypeName ⇒ false
    }

    if (isConvertible(syntaxAST)) {
      codeHandler <<
        ArgLoad(1) <<
        InvokeStatic(WRAPPER_CLASS, "mkString", s"($OBJECT_TYPE)$STRING_TYPE") <<
        InvokeStatic("java/lang/Float", "valueOf", s"($STRING_TYPE)Ljava/lang/Float;") <<
        InvokeVirtual("java/lang/Float", "floatValue", "()F")
    } else {
      throw new GeneratorError("Variable cannot automatically be mapped to Float")
    }
  }

  private def generate(): Unit = {
    val mapCH = pbClass.addMethod(OBJECT_TYPE, "map", OBJECT_TYPE).codeHandler
    //val freshVars = matterType.mappings map (mapCH.getFreshVar)
    mapCH <<
      New(matterType.toJavaClass) <<
      DUP

    matterType.mappings map {
      case MappingExpression(syntaxVar, mappedVar, _, op) ⇒
        // Match the AST and check if the syntax variable exists in one "match"
        (op, matterType.syntax find { case (varName, _) ⇒ syntaxVar == varName }) match {
          // CONSIDER: if varType == *Type*
          case (StringMapping, Some((_, ast))) ⇒ generateStringMapping(mappedVar, ast, mapCH)
          case (FloatMapping, Some((_, ast)))  ⇒ generateFloatMapping(mappedVar, ast, mapCH)
          case _                               ⇒ throw new GeneratorError(s"Variable not found: $syntaxVar")
        }
      case MappingStatement(syntaxVar, mappedVar, _, ops) ⇒ ops foreach { op ⇒
        op match {
          case CallExpression(caller, callee) ⇒
            generateCall(caller, callee, mapCH)
          case _ ⇒ throw new GeneratorError(s"Not implemented yet: $op")
        }
      }
    }

    val matterTypeMembers = matterType.mappings.map(_.varType.toJavaType).toList
    mapCH <<
      InvokeSpecial(matterType.toJavaClass, "<init>", s"(${matterTypeMembers.mkString})V") <<
      ARETURN

    mapCH.freeze
  }
}

object MappingGenerator {
  def apply(
    matterType: MatterType,
    pbClass: ClassFile
  )(
    implicit
    config: Config,
    pkg: Package
  ) = new MappingGenerator(matterType, pbClass).generate
}
