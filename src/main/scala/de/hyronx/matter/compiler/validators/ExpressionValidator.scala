package de.hyronx.matter.compiler.validators

import de.hyronx.matter.compiler.ast.{ AST, Assign, BodyAST, Declare, TypeVariable, Variable ⇒ ASTVariable }
import de.hyronx.matter.compiler.errors.{ UnknownVariable, ValidatorError }
import de.hyronx.matter.compiler.types._
import de.hyronx.matter.library.MutableTree

class ExpressionValidator private (typ: TypeTraitNode) {
  /* TODO: Use of null; consider safety */
  private var typeCache: TypeTraitNode = _

  def validate(ast: Seq[AST]): Unit = ast.headOption.foreach {
    case BodyAST(Assign(ASTVariable(name, optType)), op) ⇒ typ.variables
      .find {
        case VariableLike(varName, varType) ⇒
          optType.map { typeName ⇒
            varName == name && varType == typeName
          }

            .getOrElse {
              //typeCache = op.getType(typ)
              //varType == typeCache
              throw ValidatorError(UnknownVariable(varName, typ))
            }
        //case VariableLike(varName, None) ⇒ varName == name && op == None
      }
      .map { variable ⇒
        println(s"ExpressionValidator:validate! Variable found: $variable")
        validate(ast.tail)
      }
      .getOrElse {
        println(s"ExpressionValidator:validate! Variable not found but type generated: $typeCache")
        validate(ast.tail)
      }
    case TypeVariable(name, optType) ⇒
      println(s"ExpressionValidator:validate! Called for TypeVariable $name")
      typ match {
        case typ: GenericUserTypeTrait ⇒
          // Get type bound by finding the type from TypeName if available
          val bound = optType.map(_.toType()).getOrElse(Types.matter)
          typ.genericBounds = typ.genericBounds +
            SingleGenericParameter(name, bound)

          validate(ast.tail)
        case _ ⇒
      }
    case unknown ⇒
      println(s"ExpressionValidator:validate! AST not handled: $unknown")
      validate(ast.tail)
  }
}

object ExpressionValidator {
  def apply(typ: TypeTraitNode, astList: Seq[AST]): ExpressionValidator = {
    val validator = apply(typ)
    validator.validate(astList)
    validator
  }

  def apply(typ: TypeTraitNode): ExpressionValidator = new ExpressionValidator(typ)

  def validateDeclares(ast: Seq[AST]): Seq[AST] = ast.headOption.flatMap {
    case decl @ Declare(ASTVariable(declName, declOptType)) ⇒ ast.tail
      .find {
        case Assign(ASTVariable(varName, varOptType)) ⇒
          println(s"ExpressionValidator:validateDeclares! varName: $varName ? declName: $declName")
          println(s"ExpressionValidator:validateDeclares! varOptType: $varOptType ? declOptType: $declOptType")
          declName == varName && declOptType == varOptType
        case _ ⇒ false
      }
      .map { _ ⇒
        val (first, second) = ast.splitAt(ast.indexOf(decl))
        validateDeclares(first.dropRight(1) ++ second)
      }
    case _ ⇒ None
  }.getOrElse(ast)

  def validateGenericTypeDeclarations(
    child: GenericTypeTrait,
    typeDecls: Seq[TypePath]
  ): GenericParams = {
    val genericBounds = child.genericBounds
    // Convert `typeDecls` in generic parameters
    val typeParams: GenericParams = (genericBounds, typeDecls).zipped.map {
      case (GenericParameterTrait(name), declName) ⇒ SingleGenericParameter(name, declName.toType())
    }

    // Check if these match with the generic type bounds
    (genericBounds, typeParams).zipped
      .foreach {
        case (SingleGenericParameter(_, boundType),
          SingleGenericParameter(_, paramType)) ⇒
          if (!paramType.isAncestor(boundType))
            throw ValidatorError(s"Generic bounds do not match for: $child; Param type: $paramType; Bounds: $boundType")
        case _ ⇒ /* TODO: Implement other */
      }
    typeParams
  }
}
