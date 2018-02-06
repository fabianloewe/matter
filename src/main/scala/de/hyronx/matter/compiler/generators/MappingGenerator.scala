package de.hyronx.matter.compiler.generators

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe._
import de.hyronx.matter.Config
import de.hyronx.matter.compiler.ast.{Variable ⇒ ASTVariable, Option ⇒ ASTOption, _}
import de.hyronx.matter.compiler.errors._
import de.hyronx.matter.compiler.types._
import de.hyronx.matter.library.MutableTree

class MappingGenerator(
                        typ: UserTypeTrait,
                        classFile: ClassFile
                      )(
                        implicit
                        config: Config,
                        pkg: PackageManager
                      ) {
  private def generate(): Unit = typ.getMapping().map { mapping ⇒
    val mappingClass = pkg.addClass(typ.name + "$Mapping")
    mappingClass.addInterface(SCALA_FUNCTION1_CLASS)
    mappingClass.addDefaultConstructor
    val mapCH = mappingClass.addMethod(OBJECT_TYPE, "apply", OBJECT_TYPE).codeHandler

    def setVariableTypes(that: UserTypeTrait): Unit = that.parent match {
      case Some(parent: UserTypeTrait) ⇒
        println(s"Mapping:setVariableTypes! Current type: $that")
        val typePath = that.toPath.resolveSibling("Syntax")
        println(s"Mapping:setVariableTypes! Searched type: $typePath")
        parent
          .findChild(_ == typePath)
          .foreach { case UserTypeTrait(_, _, syntaxVars, _, _, _) ⇒ that.variables = syntaxVars }
      case Some(parent) ⇒ throw GeneratorError(s"The parent of $that is not a user-defined type and may not contain a Mapping definition")
      case None ⇒ throw GeneratorError("It was tried to generate the root type which should never happen")
    }

    def getGrammarVariable(varName: String) = mapping.variables
      .find(_.name == varName)
      .collectFirst {
        case Variable(varName, varType: GenericTypeImpl, varAst) if varType.basedOn == Types.grammar ⇒
          Variable(varName, varType, varAst)
      }

    def getGrammar(varName: String) = getGrammarVariable(varName).collectFirst { case Variable(_, _, Some(ast)) ⇒ ast }

    def getGrammarResultType(varName: String) = getGrammarVariable(varName).collectFirst {
      case Variable(_, GenericTypeImpl(_, _, _, genParams, _, _), _) ⇒ genParams.collectFirst {
        case SingleGenericParameter(_, genType) ⇒ genType
      }
    }.flatten

    def generateStringMapping(varName: String): Unit = {
      def isConvertible(ast: AST): Boolean = ast match {
        case Enclosing(defs) ⇒ isConvertible(defs)
        case BodyAST(_, defs) ⇒ defs.forall(isConvertible)
        case Literal(_) | Range(_, _) ⇒ true
        case VariableUsage(varName) ⇒ getGrammar(varName).forall(isConvertible)
        case _: TypeName ⇒ false
      }

      if (getGrammar(varName).forall(isConvertible)) {
        mapCH <<
          ArgLoad(1) <<
          InvokeStatic(WRAPPER_CLASS, "mkString", s"($OBJECT_TYPE)$STRING_TYPE")
      } else {
        throw GeneratorError(s"Variable $varName cannot automatically be mapped to String")
      }
    }

    def generateFloatMapping(varName: String): Unit = {
      var pointFound = false

      def isConvertible(ast: AST): Boolean = ast match {
        case Enclosing(defs) ⇒ isConvertible(defs)
        case BodyAST(_, defs) ⇒ defs.forall(isConvertible)
        case Literal(string) ⇒
          if ((string contains '.') && !pointFound) {
            pointFound = true
            true
          } else if ((string contains '.') && pointFound) {
            false
          } else {
            string matches "[0-9]+"
          }
        case Range(from, to) ⇒ (from matches "[0-9]+") && (to matches "[0-9]+")
        case VariableUsage(varName) ⇒ getGrammar(varName).forall(isConvertible)
        case _: TypeName ⇒ false
      }

      if (getGrammar(varName).forall(isConvertible)) {
        mapCH <<
          ArgLoad(1) <<
          InvokeStatic(WRAPPER_CLASS, "mkString", s"($OBJECT_TYPE)$STRING_TYPE") <<
          InvokeStatic(Types.float.toJavaClass, "valueOf", s"($STRING_TYPE)${Types.float.toJavaType}")
      } else {
        throw GeneratorError(s"Variable ´$varName´ cannot automatically be mapped to Float")
      }
    }

    def generateIntegerMapping(varName: String): Unit = {
      def isConvertible(ast: AST): Boolean = ast match {
        case Enclosing(defs) ⇒ isConvertible(defs)
        case BodyAST(_, defs) ⇒ defs.forall(isConvertible)
        case Literal(string) ⇒ string matches "[0-9]+"
        case Range(from, to) ⇒ (from matches "[0-9]+") && (to matches "[0-9]+")
        case VariableUsage(`varName`) ⇒ getGrammar(varName).forall(isConvertible)
        case _: TypeName ⇒ false
      }

      if (getGrammar(varName).forall(isConvertible)) {
        mapCH <<
          ArgLoad(1) <<
          InvokeStatic(WRAPPER_CLASS, "mkString", s"($OBJECT_TYPE)$STRING_TYPE") <<
          InvokeStatic(Types.integer.toJavaClass, "valueOf", s"($STRING_TYPE)${Types.integer.toJavaType}")
      } else {
        throw GeneratorError(s"Variable ´$varName´ cannot automatically be mapped to Int")
      }
    }

    def generateTypeMapping(fromVarName: String, toVarName: String, toVarTypePath: TypePath) = {
      val toVarType = toVarTypePath.getType(typ.parent)
        // Otherwise check if `toVarTypePath` is a generic parameter
        .orElse(typ match {
        case GenericTypeImpl(_, _, _, genParams, _, _) ⇒ genParams.find(_.name == toVarTypePath.name)
          .collect { case SingleGenericParameter(_, genType) ⇒ genType }
        case _ ⇒ None
      })
        .getOrElse(throw GeneratorError(s"Type $toVarTypePath could not be found"))

      if (toVarType == Types.string)
        generateStringMapping(fromVarName)
      else if (toVarType == Types.float)
        generateFloatMapping(fromVarName)
      else if (toVarType == Types.integer)
        generateIntegerMapping(fromVarName)

      Some(Variable(toVarName, toVarType))
    }

    // TODO: Merge with `generateMapExpression`
    def generateCall(fromVarName: String, toVarName: String, call: ChainedCall) = {
      val resultType = getGrammarResultType(fromVarName).getOrElse(
        throw GeneratorError(s"Variable `$fromVarName` is not of type Grammar")
      )

      def validateCaller(caller: String) = (caller, resultType) match {
        // Check if the caller is destructured variable
        case (DefaultVarPattern(index), genImpl: GenericTypeImpl) if genImpl.basedOn == Types.tuple ⇒
          genImpl.variables.exists(_ == index)
        case ("_", _) ⇒ true
      }

      def getCalleeType(callee: String) = resultType.variables.find(_ == callee).map(_.varType)

      call match {
        case ChainedCall(caller, VariableCallee(callee)) if validateCaller(caller) ⇒ getCalleeType(callee)
          .flatMap { toVarType ⇒
            println(s"MappingGenerator:generate! Function call `$caller.$callee` validated")
            mapCH <<
              Comment(s"Load $callee from Tuple") <<
              ALoad(arg1) <<
              Ldc(callee.toInt) <<
              InvokeInterface(SCALA_LIST_CLASS, "apply", s"(I)$OBJECT_TYPE") <<
              CheckCast(toVarType.toJavaClass)
            mapCH.print

            Some(Variable(toVarName, toVarType))
          }
          .orElse(throw GeneratorError(InvalidFunctionCall(call)))
        case _ ⇒
          throw GeneratorError(InvalidFunctionCall(call))
      }
    }

    def generateMethodDefinition(fromVarName: String, toVarName: String, params: Seq[ASTVariable], body: Stream[AST]) = {
      val fromVarType = getGrammarResultType(fromVarName)
        .collectFirst { case g: GenericTypeImpl ⇒ g }
        .getOrElse(throw GeneratorError(UnknownVariable(fromVarName, typ)))

      // Extract types from generic bounds
      val genericTypes = fromVarType.genericBounds.toSeq.flatMap {
        case VariadicGenericParameter(_, varTypes) ⇒ varTypes
      }

      // Zip parsed parameters with known generic bounds and index and
      // only take the wanted parameters
      val test = (params, genericTypes, Stream from 0).zipped
        .map {
          case (ASTVariable(paramName, Some(declType)), genType, index) if declType == genType ⇒
            (paramName, genType, index)
          case (ASTVariable(paramName, None), genType, index) ⇒
            (paramName, genType, index)
          case (ASTVariable(paramName, _), genType, _) ⇒
            throw GeneratorError(TypeDoesNotMatch(paramName, genType))
        }

      val destructVars = test.filterNot(_._1 == "_")

      val freshVars = {
        // TODO: Check
        // Check if there are unused variables (so it's a destructuring) and
        // if `fromVarType` is a tuple
        if (destructVars.lengthCompare(params.length) < 0 && fromVarType.basedOn == Types.tuple) {
          destructVars.map {
            case (destructVarName, destructVarType, index) ⇒
              val varIndex = mapCH.getFreshVar(destructVarType.toJavaType)

              mapCH <<
                Comment(s"Init variable $destructVarName from Tuple") <<
                ALoad(arg1) <<
                Ldc(index) <<
                InvokeInterface(SCALA_LIST_CLASS, "apply", s"(I)$OBJECT_TYPE") <<
                CheckCast(destructVarType.toJavaClass) <<
                AStore(varIndex)

              JVMVariable(destructVarName, destructVarType, varIndex)
          }
        } else Seq()
      }

      val ExpressionsGenerator.ReturnType(toVarType) = ExpressionsGenerator(typ, mapCH, body, freshVars)
      Some(Variable(toVarName, toVarType))
    }

    def generateMapExpression(fromVarName: String, toVarName: String, ast: AST) = {
      val resultType = getGrammarResultType(fromVarName).getOrElse(
        throw GeneratorError(s"Variable `$fromVarName` is not of type Grammar")
      )

      def getVariableType(variable: String) = (variable, resultType) match {
        // Check if the caller is destructured variable
        case (DefaultVarPattern(index), genImpl: GenericTypeImpl) if genImpl.basedOn == Types.tuple ⇒
          genImpl.variables.find(_ == index).map(_.varType)
        //case ("_", _) ⇒ true
        case (name, _) ⇒ typ.variables.find(_ == name).map(_.varType)
        case _ ⇒ None
      }

      def collectVariables(ast: AST): Seq[Variable[TypeTrait]] = ast match {
        case BodyAST(_, children) ⇒ children.flatMap(collectVariables)
        case ASTVariable(name, _) ⇒ Seq(Variable(
          name,
          getVariableType(name).getOrElse(
            throw GeneratorError(UnknownVariable(name))
          )
        ))
      }

      val freshVars = collectVariables(ast).map {
        case VariableLike(DefaultVarPattern(matchedIndex), varType) ⇒
          val index = Integer.valueOf(matchedIndex)
          val varIndex = mapCH.getFreshVar(varType.toJavaType)

          mapCH <<
            Comment("Init variable $" + matchedIndex + " from Tuple") <<
            ALoad(arg1) <<
            Ldc(index) <<
            InvokeInterface(SCALA_LIST_CLASS, "apply", s"(I)$OBJECT_TYPE") <<
            CheckCast(varType.toJavaClass) <<
            AStore(varIndex)

          JVMVariable("$" + matchedIndex, varType, varIndex)
        /* TODO: Implement */
        //case VariableLike(name, varType) =>
      }

      val ExpressionsGenerator.ReturnType(toVarType) = ExpressionsGenerator(typ, mapCH, ast, freshVars)
      Some(Variable(toVarName, toVarType))
    }

    // Variable holding the first argument as Scala List
    lazy val arg1 = mapCH.getFreshVar(SCALA_LIST_TYPE)
    mapCH <<
      ArgLoad(1) <<
      CheckCast(SCALA_LIST_CLASS) <<
      AStore(arg1) <<
      New(classFile.className) <<
      DUP

    setVariableTypes(mapping)

    val mappedVars: Seq[VariableLike] = mapping.body.flatMap {
      case MutableTree(Assign(ASTVariable(toVarName, _)), Stream(MapType(fromVarName, toVarTypePath))) ⇒
        generateTypeMapping(fromVarName, toVarName, toVarTypePath)
      case MutableTree(Assign(ASTVariable(toVarName, _)), Stream(MapCall(fromVarName, call))) ⇒
        generateCall(fromVarName, toVarName, call)
      case MutableTree(Assign(ASTVariable(toVarName, _)), Stream(MutableTree(MapMethodDef(fromVarName, params), body))) ⇒
        generateMethodDefinition(fromVarName, toVarName, params, body)
      case MutableTree(Assign(ASTVariable(toVarName, _)), Stream(MutableTree(MapExpression(fromVarName), Stream(ast)))) ⇒
        generateMapExpression(fromVarName, toVarName, ast)
      case value ⇒
        println(s"MappingGenerator:generate! Mapping value: $value")
        None
    }

    mapping.variables = mapping.variables ++ mappedVars
    typ.variables = typ.variables ++ mappedVars

    val typeMembers = mappedVars.map(_.varType.toJavaType)
    if (typeMembers.lengthCompare(mappedVars.length) != 0)
      throw GeneratorError("Not all types in Mapping could correctly be inferred")

    mapCH <<
      InvokeSpecial(classFile.className, "<init>", s"(${typeMembers.mkString})V") <<
      ARETURN

    mapCH.freeze

  }
}

object MappingGenerator {
  def apply(
             typ: UserTypeTrait,
             pbClass: ClassFile
           )(
             implicit
             config: Config,
             pkg: PackageManager
           ): Unit = new MappingGenerator(typ, pbClass).generate()
}
