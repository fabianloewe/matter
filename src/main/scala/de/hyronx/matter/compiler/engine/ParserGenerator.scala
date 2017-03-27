package de.hyronx.matter.compiler.generators

import scala.collection.mutable.Map

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._

import fastparse.all.P

import de.hyronx.matter.compiler.Generator
import de.hyronx.matter.compiler.types.Type
import de.hyronx.matter.compiler.ast._
import de.hyronx.matter.compiler.generators.Helpers._

class ParserGenerator(matterType: MatterType, classFile: ClassFile) {
  //private val syntaxVarNames = matterType.syntax map { case (name, _) => name }

  private val fpParserType = "Lfastparse/core/Parser;"
  private val fpWrapperName = "de/hyronx/matter/support/FastparseWrapper"
  private val optWrapper = s"($fpParserType)$fpParserType"
  private val repWrapper = s"(${fpParserType}I${fpParserType}II)$fpParserType"

  def generateParser(
    codeHandler: CodeHandler,
    ast: AST
  ): Unit = {
    def generateMultipleParsers(defs: Seq[AST], funcName: String) = {
      val arrayVar = codeHandler.getFreshVar
      codeHandler <<
        Ldc(defs.length) <<
        NewArray(fpParserType) <<
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
        InvokeStatic(fpWrapperName, funcName, MethodSig1[Seq[_], P[_]])
    }

    ast match {
      case Option(option) ⇒
        println("ParserGenerator:generateParser! Creating optional parser")
        generateParser(codeHandler, option)
        codeHandler <<
          Comment("Create optional parser") <<
          InvokeStatic(fpWrapperName, "opt", optWrapper) <<
          Comment("Created optional parser")
      case Repeat(rep) ⇒
        println("ParserGenerator:generateParser! Creating repetition parser")
        generateParser(codeHandler, rep)
        codeHandler <<
          Comment("Create repetition parser") <<
          Ldc(0) <<
          DefaultNew("fastparse/all/Pass") <<
          Ldc(0) <<
          Ldc(-1) <<
          InvokeStatic(fpWrapperName, "rep", repWrapper) <<
          Comment("Created repetition parser")
      case RepeatOne(rep) ⇒
        println("ParserGenerator:generateParser! Create simple literal parser")
        generateParser(codeHandler, rep)
        codeHandler <<
          Comment("Create repeat-once parser") <<
          Ldc(1) <<
          DefaultNew("fastparse/all/Pass") <<
          Ldc(0) <<
          Ldc(-1) <<
          InvokeStatic(fpWrapperName, "rep", repWrapper) <<
          Comment("Created repeat-once parser")
      case Range(from, to) ⇒
        println("ParserGenerator:generateParser! Creating range parser")
        codeHandler <<
          Comment("Create range parser") <<
          Ldc(from) <<
          Ldc(to) <<
          InvokeStatic(fpWrapperName, "range", MethodSig2[Char, Char, P[_]]) <<
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
          InvokeStatic(fpWrapperName, "literal", MethodSig1[String, P[_]]) <<
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
              InvokeStatic(matterTypeClass.className, "parse", s"()$fpParserType") <<
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
  def generateParserBuilder(parserName: String)(f: MethodHandler ⇒ Unit) = {
    println(s"ParserGenerator:generateParserBuilder($parserName)")
    val parserClass = new ClassFile(parserName + "Builder", None)
    parserClass.addInterface("de/hyronx/matter/support/ParserBuilder")
    parserClass.addDefaultConstructor
    val applyMethod = parserClass.addMethod("Lfastparse/core/Parser;", "apply", "V")
    f(applyMethod)
    parserClass
  }

  def generate: ClassFile = {
    val parserFlags = (Flags.FIELD_ACC_PRIVATE |
      Flags.FIELD_ACC_STATIC |
      Flags.FIELD_ACC_FINAL).asInstanceOf[Short]

    val staticInit = classFile.addMethod("V", "<clinit>", "")
    staticInit.setFlags(Flags.METHOD_ACC_STATIC)
    val staticInitCH = staticInit.codeHandler

    val mainParser = matterType.syntax.map {
      case (syntaxVar, op) ⇒
        classFile.addField(fpParserType, syntaxVar)
          .setFlags(parserFlags)

        val parserName = Helpers.capitalize(syntaxVar)

        // Initialize static parser
        staticInitCH <<
          DefaultNew(parserName + "Builder") <<
          InvokeStatic(matterType.id, "buildParser", s"(Lde/hyronx/matter/support/ParserBuilder;)$fpParserType") <<
          PutStatic(matterType.id, syntaxVar, fpParserType)

        val parserBuilder = generateParserBuilder(parserName) { methodHandler ⇒
          val methodCH = methodHandler.codeHandler

          generateParser(methodCH, op)
          methodCH << ARETURN
          methodCH.freeze
        }
        parserBuilder.writeToFile(matterType.id + "$" + parserBuilder.className + ".class")
        parserName
    }.last

    val parseMethod = classFile.addMethod(fpParserType, "parse", "")
    parseMethod.setFlags((Flags.METHOD_ACC_PUBLIC | Flags.METHOD_ACC_STATIC).asInstanceOf[Short])
    parseMethod.codeHandler <<
      GetStatic(classFile.className, mainParser, fpParserType) <<
      ARETURN
    parseMethod.codeHandler.freeze

    staticInitCH << RETURN
    staticInitCH.freeze

    classFile
  }
}

object ParserGenerator {
  def apply(matterType: MatterType, classFile: ClassFile) = {
    new ParserGenerator(matterType, classFile).generate
  }
}
