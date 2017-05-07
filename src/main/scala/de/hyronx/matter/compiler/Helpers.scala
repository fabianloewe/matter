package de.hyronx.matter.compiler

import scala.reflect._

import de.hyronx.matter.compiler.types._
import de.hyronx.matter.compiler.ast.{ AST, MatterTypeTree, MatterType, BaseBuiltIn }

object Helpers {
  private[compiler] val PARSER_CLASS = "fastparse/core/Parser"
  private[compiler] val PARSER_TYPE = s"L${PARSER_CLASS};"
  private[compiler] val STRING_CLASS = "java/lang/String"
  private[compiler] val STRING_TYPE = s"L${STRING_CLASS};"
  private[compiler] val WRAPPER_CLASS = "de/hyronx/matter/library/FastparseWrapper"
  private[compiler] val OBJECT_TYPE = "Ljava/lang/Object;"

  implicit class VariableTypeConverter(varType: Type) {
    val toJavaClass = varType match {
      case StringType               ⇒ "java/lang/String"
      case TupleType(_) | TupleType ⇒ "scala/Product"
      case StructType(matterType)   ⇒ matterType.toJavaClass
      // Integers are encoded as Java long
      case IntType                  ⇒ "J"
      case VoidType                 ⇒ "V"
      case BoolType                 ⇒ "Z"
      case FloatType                ⇒ "F"
    }

    val toJavaType = {
      if (toJavaClass.length > 1)
        s"L$toJavaClass;"
      else
        toJavaClass
    }
  }

  implicit class MatterTypeConverter(matterType: MatterType) {
    val toJavaClass = {
      def constructHierarchy(mType: MatterTypeTree): List[String] = {
        if (mType == BaseBuiltIn)
          List(BaseBuiltIn.id)
        else
          mType.id :: constructHierarchy(mType.parent)
      }

      constructHierarchy(matterType).reverse.mkString("/")
    }

    val toJavaType = s"L$toJavaClass;"
  }

  implicit class ASTConverter(ast: AST) {
    import de.hyronx.matter.compiler.ast._

    def getType(syntaxVars: AST.SyntaxMap): Type = {
      def matchSyntax(ast: AST) = ast match {
        case Option(internalDefs)    ⇒ OptionalType(internalDefs.getType(syntaxVars))
        case Repeat(internalDefs)    ⇒ ListType(internalDefs.getType(syntaxVars))
        case RepeatOne(internalDefs) ⇒ ListType(internalDefs.getType(syntaxVars))
        case Literal(string)         ⇒ StringType
        case Range(from, to)         ⇒ StringType
        case Concatenation(defs)     ⇒ TupleType(matchTupleTypes(defs))
        case Selection(defs)         ⇒ matchSelection(defs)
        case Declaration(syntaxType) ⇒ syntaxType match {
          case SyntaxType.String  ⇒ StringType
          case SyntaxType.Grammar ⇒ TupleType
        }
        case VariableUsage(varName) ⇒ syntaxVars get varName match {
          case Some(defs) ⇒ defs.getType(syntaxVars)
          case None       ⇒ throw new ParserError(s"No such variable: $varName")
        }
        case matterType: MatterType ⇒ StructType(matterType)
        case typeName: TypeName     ⇒ Type(typeName) getOrElse UnknownType
        case other ⇒
          println(s"MappingParser:matchSyntax! Other: $other")
          VoidType
      }

      def matchTupleTypes(ast: Seq[AST]): List[Type] = ast.headOption match {
        case Some(head) ⇒ matchSyntax(head) :: matchTupleTypes(ast.tail)
        case None       ⇒ List()
      }

      def matchSelection(ast: Seq[AST]): Type = {
        val types = ast map (_.getType(syntaxVars))
        if (types forall (_ == types.head)) {
          types.head
        } else {
          UnionType(types)
        }
      }

      matchSyntax(ast)
    }
  }
}
