package de.hyronx.matter.compiler.generators

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._

import de.hyronx.matter.Config
import de.hyronx.matter.compiler.Generator
import de.hyronx.matter.compiler.Helpers._
import de.hyronx.matter.compiler.ast._

class ParserGenerator(
    matterType: MatterType,
    classFile: ClassFile
)(
    implicit
    config: Config,
    pkg: PackageManager
) {
  //private val className = matterType.id + "$" + parserName + "Builder"

  private val PARSER_CLASS = "fastparse/core/Parser"
  private val PARSER_TYPE = s"L${PARSER_CLASS};"
  private val WRAPPER_CLASS = "de/hyronx/matter/library/FastparseWrapper"
  private val OPT_WRAPPER_SIG = s"($PARSER_TYPE)$PARSER_TYPE"
  private val REP_WRAPPER_SIG = s"(${PARSER_TYPE}ILscala/Option;II)$PARSER_TYPE"

  def generateParser(codeHandler: CodeHandler, ast: AST): Unit = {
    def generateMultipleParsers(defs: Seq[AST], funcName: String) = {
      val arrayVar = codeHandler.getFreshVar(s"[$PARSER_TYPE")
      codeHandler <<
        Ldc(defs.length) <<
        NewArray(PARSER_CLASS) <<
        AStore(arrayVar) <<
        Comment("ParserGenerator:generateMultipleParsers! children follow")

      defs.zipWithIndex.foreach {
        case (definition, i) ⇒
          codeHandler <<
            ALoad(arrayVar) <<
            Ldc(i)
          generateParser(codeHandler, definition)
          codeHandler << AASTORE
      }
      codeHandler <<
        Comment("ParserGenerator:generateMultipleParsers! children called") <<
        ALoad(arrayVar) <<
        InvokeStatic(WRAPPER_CLASS, funcName, s"([$PARSER_TYPE)$PARSER_TYPE")
    }

    ast match {
      case Option(option) ⇒
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
      case Concatenation(defs) ⇒
        println("ParserGenerator:generateParser! Creating concatenation parser")
        generateMultipleParsers(defs, "concat")
      case Selection(defs) ⇒
        println("ParserGenerator:generateParser! Creating selection parser")
        generateMultipleParsers(defs, "or")
      case Literal(string) ⇒
        println("ParserGenerator:generateParser! Creating simple literal parser")
        codeHandler <<
          Comment("Create simple literal parser") <<
          Ldc(string) <<
          InvokeStatic(WRAPPER_CLASS, "literal", s"(Ljava/lang/String;)$PARSER_TYPE") <<
          Comment("Created simple literal parser")
      case VariableUsage(varName) ⇒
        matterType.syntax find { case (syntaxVar, _) ⇒ syntaxVar == varName } match {
          case Some((_, syntaxAST)) ⇒ generateParser(codeHandler, syntaxAST)
          case None                 ⇒ println(s"ParserGenerator:generateParser! Variable is nowhere defined: $varName")
        }
      case id: TypeName ⇒
        this.matterType.find(id) match {
          case Some(matterType: MatterType) ⇒
            val matterTypeClass = Generator.generate(matterType)

            codeHandler <<
              Comment("Create Matter type parser") <<
              InvokeStatic(matterTypeClass.className, "parse", s"()$PARSER_TYPE") <<
              Comment("Created Matter type parser")
          case Some(other) ⇒
            println(s"ParserGenerator:generateParser! Got matter type: $other")
          case None ⇒
            println(s"ParserGenerator:generateParser! MatterType not found for $ast")
        }
      case unknown ⇒ println(s"ParserGenerator:generateParser! Parser not known: $unknown")
    }
    /*optVar match {
      case Some(varNum) ⇒ codeHandler << AStore(varNum)
      case None         ⇒
    }*/
  }

  // @param parserName Should be the capitalized name of the generated parser
  def generateParserBuilder(parserName: String, op: AST): Unit = {
    println(s"ParserGenerator:generateParserBuilder($parserName)")
    val parserClass = pkg.addClass(matterType.id + "$" + s"${parserName}Builder", None)
    parserClass.addInterface("de/hyronx/matter/library/ParserBuilder")
    parserClass.addDefaultConstructor

    val methodCH = parserClass.addMethod(PARSER_TYPE, "apply").codeHandler
    generateParser(methodCH, op)
    methodCH << ARETURN
    methodCH.freeze

    MappingGenerator(matterType, parserClass)
  }

  def generate: ClassFile = {
    val parserFlags = (
      Flags.FIELD_ACC_PRIVATE |
      Flags.FIELD_ACC_STATIC |
      Flags.FIELD_ACC_FINAL
    ).asInstanceOf[Short]

    val staticInitCH = classFile.addStaticBlock.codeHandler

    val mainParser = matterType.syntax.map {
      case (syntaxVar, op) ⇒
        val parserVar = syntaxVar + "Parser"
        classFile.addField(PARSER_TYPE, parserVar)
          .setFlags(parserFlags)

        val parserName = syntaxVar.capitalize

        // Initialize static parser
        staticInitCH <<
          DefaultNew(classFile.className + "$" + parserName + "Builder") <<
          InvokeStatic(WRAPPER_CLASS, "buildParser", s"(Lde/hyronx/matter/library/ParserBuilder;)$PARSER_TYPE") <<
          PutStatic(classFile.className, parserVar, PARSER_TYPE)

        generateParserBuilder(parserName, op)
        parserVar
    }.last

    val parseMethod = classFile.addMethod(PARSER_TYPE, "parse")
    parseMethod.setFlags((Flags.METHOD_ACC_PUBLIC | Flags.METHOD_ACC_STATIC).asInstanceOf[Short])
    parseMethod.codeHandler <<
      GetStatic(classFile.className, mainParser, PARSER_TYPE) <<
      ARETURN
    parseMethod.codeHandler.freeze

    staticInitCH << RETURN
    staticInitCH.freeze

    classFile
  }
}

object ParserGenerator {
  def apply(matterType: MatterType, classFile: ClassFile)(implicit config: Config, pkg: PackageManager) = {
    new ParserGenerator(matterType, classFile).generate
  }
}
