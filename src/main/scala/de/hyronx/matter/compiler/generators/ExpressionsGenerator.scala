package de.hyronx.matter.compiler.generators

import cafebabe.AbstractByteCodes._
import cafebabe._
import de.hyronx.matter.compiler.ast.{ Option ⇒ ASTOption, Variable ⇒ ASTVariable, _ }
import de.hyronx.matter.compiler.errors._
import de.hyronx.matter.compiler.types._

class ExpressionsGenerator(
    typ: UserTypeTrait,
    codeHandler: CodeHandler
) {

  import ExpressionsGenerator._

  def generate(
    ast: AST,
    jvmVars: Seq[JVMVariable]
  ): ReturnType = {
    def generateLoadVariable(varName: String) = {
      jvmVars
        .find(_.name == varName)
        .map {
          case JVMVariable(_, varType, varIndex) ⇒
            codeHandler << ALoad(varIndex)

            // Call `getValue()` implicitly if `jvmVar`'s type has a variable `value` defined
            varType.variables
              .find(_.name == "value")
              .map {
                case VariableLike(_, valueType) ⇒
                  codeHandler << InvokeVirtual(varType.toJavaClass, "getValue", s"()${valueType.toJavaType}")
                  ReturnType(valueType)
              }
              .getOrElse(ReturnType(varType))
        }
        .getOrElse(throw GeneratorError(UnknownVariable(varName, typ)))
    }

    def generateLiteral(lit: String) = {
      codeHandler << Ldc(lit)
      ReturnType(Types.string)
    }

    def generateConcatenation(defs: Seq[AST]) = {
      codeHandler <<
        Comment("Init StringBuilder") <<
        DefaultNew(STRING_BUILDER_CLASS)

      defs.foreach { definition ⇒
        val ReturnType(varType) = generate(definition, jvmVars)

        codeHandler <<
          Comment(s"Append $varType to String") <<
          //InvokeVirtual(varType.toJavaClass, "toString", s"()$STRING_TYPE") <<
          InvokeVirtual(STRING_BUILDER_CLASS, "append", s"($STRING_TYPE)$STRING_BUILDER_TYPE")
      }

      codeHandler <<
        Comment("Finish StringBuilder") <<
        InvokeVirtual(STRING_BUILDER_CLASS, "toString", s"()$STRING_TYPE")

      ReturnType(Types.string)
    }

    def generateChainedCall(call: ChainedCall) = {
      @scala.annotation.tailrec
      def generateCall(
        call: Callee,
        callerType: TypeTrait
      ): ReturnType = {
        //println(s"ExpressionGenerator:generate! Got Call: $caller, $callee, $params")
        def getMemberType(member: String) = callerType.variables
          .find(_.name == member)
          .map(_.varType)
          .getOrElse(throw GeneratorError(UnknownVariable(member, callerType)))

        call match {
          case ChainedCall(_, VariableCallee(varName)) ⇒
            val resultType = getMemberType(varName)
            codeHandler <<
              Comment(s"$callerType.$varName") <<
              InvokeVirtual(callerType.toJavaClass, varName, s"()${resultType.toJavaType}")
            ReturnType(resultType)

          case ChainedCall(_, Call(callee, params)) ⇒
            val resultType = getMemberType(callee)
            val paramTypes = params.map(generate(_, jvmVars)).map { case ReturnType(retType) ⇒ retType }

            codeHandler <<
              Comment(s"$callerType.$callee(${paramTypes.mkString(", ")})") <<
              InvokeVirtual(callerType.toJavaClass, callee, s"(${paramTypes.map(_.toJavaType).mkString("")})${resultType.toJavaType}")
            ReturnType(resultType)
          case ChainedCall(_, callee: ChainedCall) ⇒
            val memberType = getMemberType(callee.name)

            codeHandler <<
              Comment(s"$callerType.$callee") <<
              InvokeVirtual(callerType.toJavaClass, callee.name, s"()${memberType.toJavaType}")
            generateCall(callee, memberType)
        }
      }

      val jvmVar = jvmVars.find(_.name == call.name).getOrElse(throw GeneratorError(UnknownVariable(call.name, typ)))
      codeHandler << ALoad(jvmVar.jvmIndex)

      generateCall(call, jvmVar.varType)
    }

    ast match {
      case VariableUsage(varName)          ⇒ generateLoadVariable(varName)
      case ASTVariable(varName, None)      ⇒ generateLoadVariable(varName)
      case ExpressionList(list)            ⇒ list.map(generate(_, jvmVars)).last
      case Literal(lit)                    ⇒ generateLiteral(lit)
      case BodyAST(_: Concatenation, defs) ⇒ generateConcatenation(defs)
      case call: ChainedCall               ⇒ generateChainedCall(call)
      //case other => println(s"ExpressionGenerator:generate! Got $other")
    }
  }
}

object ExpressionsGenerator {

  def apply(
    typ: UserTypeTrait,
    codeHandler: CodeHandler
  ) = new ExpressionsGenerator(typ, codeHandler)

  def apply(
    typ: UserTypeTrait,
    codeHandler: CodeHandler,
    ast: AST,
    jvmVars: Seq[JVMVariable]
  ): ReturnType = new ExpressionsGenerator(typ, codeHandler).generate(ast, jvmVars)

  def apply(
    typ: UserTypeTrait,
    codeHandler: CodeHandler,
    ast: Stream[AST],
    jvmVars: Seq[JVMVariable]
  ): ReturnType = new ExpressionsGenerator(typ, codeHandler).generate(ExpressionList(ast), jvmVars)

  case class ReturnType(typ: TypeTrait)

}