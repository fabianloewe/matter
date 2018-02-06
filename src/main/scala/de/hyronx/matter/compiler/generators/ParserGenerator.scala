package de.hyronx.matter.compiler.generators

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._
import de.hyronx.matter.library.MutableTree
import de.hyronx.matter.Config
import de.hyronx.matter.compiler.types._
import de.hyronx.matter.compiler.Generator
import de.hyronx.matter.compiler.ast.{Option ⇒ ASTOption, Variable ⇒ ASTVariable, _}
import de.hyronx.matter.compiler.errors.GeneratorError

class ParserGenerator(
                       typ: UserTypeTrait,
                       classFile: ClassFile
)(
    implicit
    config: Config,
    pkg: PackageManager
) {
  //import ParserGenerator._

  private def generateParser(codeHandler: CodeHandler, ast: AST): Unit = {
    def generateMultipleParsers(defs: Seq[AST], funcName: String) = {
      val arrayVar = codeHandler.getFreshVar(s"[$PARSER_TYPE")
      codeHandler <<
        Ldc(defs.length) <<
        NewArray(PARSER_CLASS) <<
        AStore(arrayVar) <<
        Comment("ParserGenerator:generateMultipleParsers! children follow")

      defs.zipWithIndex foreach {
        case (definition, i) ⇒
          codeHandler <<
            ALoad(arrayVar) <<
            Ldc(i) <<
            Comment(s"ParserGenerator:generateMultipleParsers! Current def: ${definition}")
          generateParser(codeHandler, definition)
          codeHandler << AASTORE
      }
      codeHandler <<
        Comment("ParserGenerator:generateMultipleParsers! children called") <<
        ALoad(arrayVar) <<
        InvokeStatic(WRAPPER_CLASS, funcName, s"([$PARSER_TYPE)$PARSER_TYPE")
    }

    ast match {
      case ASTOption(option) ⇒
        println("ParserGenerator:generateParser! Creating optional parser")
        generateParser(codeHandler, option)
        codeHandler <<
          Comment("Create optional parser") <<
          InvokeStatic(WRAPPER_CLASS, "opt", OPT_WRAPPER_SIG) <<
          Comment("Created optional parser")
      case Repeat(rep) ⇒
        println("ParserGenerator:generateParser! Creating repetition parser")
        generateParser(codeHandler, rep)
        codeHandler <<
          Comment("Create repetition parser") <<
          Ldc(0) <<
          ACONST_NULL <<
          InvokeStatic("scala/Int", "MaxValue", "()I") <<
          Ldc(-1) <<
          InvokeStatic(WRAPPER_CLASS, "rep", REP_WRAPPER_SIG) <<
          Comment("Created repetition parser")
      case RepeatOne(rep) ⇒
        println("ParserGenerator:generateParser! Create simple literal parser")
        generateParser(codeHandler, rep)
        codeHandler <<
          Comment("Create repeat-once parser") <<
          Ldc(1) <<
          ACONST_NULL <<
          Ldc(0) <<
          Ldc(-1) <<
          InvokeStatic(WRAPPER_CLASS, "rep", REP_WRAPPER_SIG) <<
          Comment("Created repeat-once parser")
      case Range(from, to) ⇒
        println("ParserGenerator:generateParser! Creating range parser")
        codeHandler <<
          Comment("Create range parser") <<
          Ldc(from) <<
          Ldc(to) <<
          InvokeStatic(WRAPPER_CLASS, "range", s"($STRING_TYPE$STRING_TYPE)$PARSER_TYPE") <<
          Comment("Created range parser")
      case BodyAST(_: Concatenation, defs) ⇒
        println("ParserGenerator:generateParser! Creating concatenation parser")
        generateMultipleParsers(defs, "concat")
      case BodyAST(_: Selection, defs) ⇒
        println("ParserGenerator:generateParser! Creating selection parser")
        generateMultipleParsers(defs, "or")
      case Literal(string) ⇒
        println(s"ParserGenerator:generateParser! Creating simple literal parser for: $string")
        codeHandler <<
          Comment("Create simple literal parser") <<
          Ldc(string) <<
          InvokeStatic(WRAPPER_CLASS, "literal", s"(Ljava/lang/String;)$PARSER_TYPE") <<
          Comment("Created simple literal parser")
      case VariableUsage(varName) ⇒
        typ.getSyntax.map(_.body collectFirst {
          case BodyAST(Assign(ASTVariable(syntaxVar, _)), syntaxAST) if syntaxVar == varName ⇒ syntaxAST
        } match {
          case Some(syntaxAST) ⇒
            //generateParser(codeHandler, syntaxAST)
            codeHandler << GetStatic(typ.toJavaClass, s"${varName}Parser", PARSER_TYPE)
          case None ⇒ println(s"ParserGenerator:generateParser! Variable is nowhere defined: $varName")
        })
      case TypeConstruction(typeName, generics, params) ⇒
        def generator(matterTypeClass: ClassFile): Unit = codeHandler <<
          Comment("Create Matter type parser") <<
          InvokeStatic(matterTypeClass.className, "getParser", s"()$PARSER_TYPE") <<
          Comment("Created Matter type parser")

        def getGenericImpl(foundType: GenericTypeTrait) = {
          // If `foundType` is already a generic implementation AND
          // `generics` just are type variables, replace them with
          // their corresponding types
          lazy val implType = typ.asInstanceOf[GenericTypeImpl]
          if (typ.isInstanceOf[GenericTypeImpl]) {
            foundType.getImplementation(implType.genericBounds)
          } else {
            foundType.getImplementation(generics.map(_.toType()))
          }
        }

        println(s"ParserGenerator:generateParser! Needs: $typeName")
        if (!generics.isEmpty) {
          (for {
            foundType ← typeName.getType(typ.parent).collect { case t: GenericTypeTrait ⇒ t }
            impl ← getGenericImpl(foundType)
            matterTypeClass = Generator.generate(impl)
          } yield matterTypeClass).map(generator(_)).getOrElse(throw GeneratorError(s"Type $typeName is unknown"))
        } else {
          (for {
            foundType ← typeName.getType(typ.parent).collect { case t: UserTypeTrait ⇒ t }
            matterTypeClass = Generator.generate(foundType)
          } yield matterTypeClass).map(generator(_)).getOrElse(throw GeneratorError(s"Type $typeName is unknown"))
        }
      case unknown ⇒ println(s"ParserGenerator:generateParser! Parser unknown: $unknown")
    }
  }

  /*
  // @param parserName Should be the capitalized name of the generated parser
  private def generateParserBuilder(parserName: String, op: AST): Unit = {
    println(s"ParserGenerator:generateParserBuilder($parserName)")
    val parserClass = pkg.addClass(typ.name + "$" + s"${parserName}Builder", None)
    parserClass.addInterface("de/hyronx/matter/library/ParserBuilder")
    parserClass.addDefaultConstructor

    //import sext._
    //println(op.treeString)

    val methodCH = parserClass.addMethod(PARSER_TYPE, "apply").codeHandler
    generateParser(methodCH, op)
    methodCH << ARETURN
    methodCH.freeze

    MappingGenerator(typ, classFile)
  }
  */

  private def generateParseMethod(parserName: String, ast: AST): Unit = {
    println(s"ParserGenerator:generateParseMethod($parserName)!")
    val parseMethod = classFile.addMethod(PARSER_TYPE, s"parse$parserName")
    parseMethod.setFlags((Flags.FIELD_ACC_PRIVATE | Flags.METHOD_ACC_STATIC).asInstanceOf[Short])

    val methodCH = parseMethod.codeHandler
    generateParser(methodCH, ast)
    methodCH << ARETURN
    methodCH.freeze
  }

  private def generate: ClassFile = {
    val parserFlags = (
      Flags.FIELD_ACC_PRIVATE |
      Flags.FIELD_ACC_STATIC |
      Flags.FIELD_ACC_FINAL
    ).asInstanceOf[Short]

    val staticInitCH = classFile.addStaticBlock.codeHandler

    //println(s"ParserGenerator:generate! Children: ${typ.children.map(_.name).mkString(", ")}")

    typ.getSyntax.map { syntax ⇒
      import GenericParams._

      def matchType(ast: AST): TypeTrait = ast match {
        case ASTOption(option) ⇒ Types.option.getImplementation(GenericParams("T" → matchType(option))).get
        case Repeat(rep) ⇒ Types.list.getImplementation(GenericParams("T" → matchType(rep))).get
        case RepeatOne(rep) ⇒ Types.list.getImplementation(GenericParams("T" → matchType(rep))).get
        case Range(from, to) ⇒ Types.string
        case BodyAST(_: Concatenation, defs) ⇒ Types.tuple.getImplementation(GenericParams("T" → defs.map(matchType))).get
        case BodyAST(_: Selection, defs) ⇒ Types.union.getImplementation(GenericParams("T" → defs.map(matchType).toSet)).get
        case Literal(string) ⇒ Types.string
        case VariableUsage(varName) ⇒ syntax.body.collectFirst {
          case BodyAST(Assign(ASTVariable(syntaxVarName, optSyntaxType)), Stream(op)) if syntaxVarName == varName ⇒
            optSyntaxType.map(_.toType()).getOrElse(matchType(op))
        }.get
        case TypeConstruction(typeName, Seq(), _) ⇒ typeName.getType(typ.parent).getOrElse(throw GeneratorError(s"Type $typeName is unknown"))
        case TypeConstruction(typeName, typePaths, _) ⇒
          // If `typ` is  a generic type implementation, try to find the corresponding type variable and
          // use its type
          def findTypeVariable(varName: String) = typ match {
            case typ: GenericTypeImpl ⇒ typ.genericBounds
              .find(_.name == varName)
              .collectFirst {
                case SingleGenericParameter(_, genType) ⇒ genType
              }
          }

          typeName.getType(typ.parent)
            .collectFirst { case genType: GenericTypeTrait ⇒ genType }
            .flatMap { genType ⇒
              val genParams = typePaths.map { typePath ⇒
                typePath.getType()
                  .orElse(findTypeVariable(typePath.name))
                  .getOrElse(throw GeneratorError(s"Type $typeName is unknown"))
              }

              genType.getImplementation(genParams)
            }
            .getOrElse(throw GeneratorError(s"Type $typeName is unknown"))
      }

      def defineGrammarType(ast: AST) = {
        Types.grammar
          .getImplementation(
            GenericParams(
              "T" → matchType(ast)
            )
          )
      }

      val syntaxVars = syntax.variables
      val mainParser = syntax.body.collect {
        case BodyAST(Assign(ASTVariable(syntaxVar, optSyntaxType)), Stream(op)) ⇒
          val syntaxType = optSyntaxType
            .map(_.toType())
            .orElse(defineGrammarType(op))
            .getOrElse(throw GeneratorError(s"The type of variable $syntaxVar could not be infered"))

          syntax.variables = syntaxVars + Variable(syntaxVar, syntaxType, Some(op))

          val parserVar = syntaxVar + "Parser"
          classFile.addField(PARSER_TYPE, parserVar)
            .setFlags(parserFlags)

          val parserName = syntaxVar.capitalize
          staticInitCH <<
            InvokeStatic(classFile.className, s"parse$parserName", s"()$PARSER_TYPE") <<
            PutStatic(classFile.className, parserVar, PARSER_TYPE)

          generateParseMethod(parserName, op)
          parserVar
      }.lastOption.getOrElse(throw GeneratorError(s"No grammar defined in Syntax of $typ"))

      val parseMethod = classFile.addMethod(PARSER_TYPE, "getParser")
      parseMethod.setFlags((Flags.METHOD_ACC_PUBLIC | Flags.METHOD_ACC_STATIC).asInstanceOf[Short])
      parseMethod.codeHandler <<
        GetStatic(classFile.className, mainParser, PARSER_TYPE)

      if (typ.hasMapping)
        parseMethod.codeHandler <<
          DefaultNew(classFile.className + "$Mapping") <<
          InvokeStatic(WRAPPER_CLASS, "map", s"($PARSER_TYPE$SCALA_FUNCTION1_TYPE)$PARSER_TYPE")

      parseMethod.codeHandler << ARETURN
      parseMethod.codeHandler.freeze
    }.orElse {
      println("ParserGenerator:generate! No Syntax found!")
      None
    }

    staticInitCH << RETURN
    staticInitCH.freeze

    classFile
  }
}

object ParserGenerator {
  def apply(typ: UserTypeTrait, classFile: ClassFile)(implicit config: Config, pkg: PackageManager) = {
    new ParserGenerator(typ, classFile).generate
  }
}
