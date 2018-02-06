package de.hyronx.matter.compiler

import de.hyronx.matter.compiler.ast.{ Variable ⇒ ASTVariable, _ }
import de.hyronx.matter.compiler.errors.ValidatorError
import de.hyronx.matter.compiler.types._
import de.hyronx.matter.compiler.validators.ExpressionValidator
import de.hyronx.matter.library.MutableTree

object Validator {

  import scala.util.Try

  type Result = Either[ValidatorError, Unit]

  //import Validator._

  def apply(
    ast: ExpressionList[AST],
    unitName: scala.Option[String] = None
  ): Either[Throwable, BuiltInNode] = {
    val parent = createTopLevelUnit(unitName).getOrElse(Types.root)
    (for {
      _ ← Try(buildTree(ast.children, parent))
      _ ← Try(Types.root.printTree(detailed = true))
      _ ← Try(println("Checking ancestors..."))
      _ ← Try(Types.matter foreach checkAncestor)
      _ ← Try(println("Defining generic types..."))
      _ ← Try(defineGenericTypes(ast.children, parent))
      //_ ← Try(println("Checking type operations..."))
      _ ← Try(checkTypeOperations(ast.children, parent))
    } yield Types.root.asInstanceOf[BuiltInNode]).toEither
  }

  private def createTopLevelUnit(unitName: scala.Option[String]) = unitName.map { name ⇒
    Types.root.addChild(
      AbstractType(
        name,
        Types.matter
      )
    )
  }

  /*
   * Check if there is a declared but not defined variable in the body OR if the body is empty
   */
  private def isAbstractType(body: Seq[AST]): Boolean = body.isEmpty || body.exists {
    case BodyAST(_, body) ⇒ isAbstractType(body)
    case _: Declare       ⇒ true
    case _                ⇒ false
  }

  /*
   * First pass: Build the type tree
   *
   * Notes:
   * Must be the first check. The following checks may be run asynchronously.
   *
   * This method throws ValidatorError because Either became unhandable
   * through the mappings.
   *
   * WARN: DON'T USE `baseNode`! It is initialized with this method!
   */
  //@scala.annotation.tailrec
  private def buildTree(
    ops: Seq[AST],
    parent: TypeTrait
  ): Unit = for (op ← ops) op match {
    // Check if it's a abstract type
    case MutableTree(TypeDefinition(typePath, ancestor, Seq(), _), body) if isAbstractType(body) ⇒
      val ancType = ancestor.toType()
      val child = AbstractType(
        typePath.name,
        ancType,
        body
      )
      ancType.children.foreach { ancChild ⇒ child.addChild(ancChild.duplicate()) }
      parent.addChild(child)
      buildTree(body, child)
    // Check if it's a abstract generic type
    // It must EITHER be abstract and have generic type declarations OR
    // have a body but no concrete type definitions for its type declarations (considered abstract until instanstiated)
    case MutableTree(TypeDefinition(typePath, ancestor, typeDecls, _), body) if isAbstractType(body) && typeDecls.flatMap(_.optType).isEmpty ⇒
      val ancType = ancestor.toType()
      val child = AbstractGenericType(
        typePath.name,
        ancestor.toType(),
        body
      )
      ancType.children.foreach { ancChild ⇒ child.addChild(ancChild.duplicate()) }
      parent.addChild(child)
      buildTree(body, child)
    // Check if it's a fully defined type
    case MutableTree(TypeDefinition(typePath, ancestor, Seq(), _), body) ⇒
      val ancType = ancestor.toType()
      val child = Type(
        typePath.name,
        ancestor.toType(),
        body
      )
      ancType.children.foreach { ancChild ⇒ child.addChild(ancChild.duplicate()) }
      parent.addChild(child)
      buildTree(body, child)
    // Check if it's a generic type
    case MutableTree(TypeDefinition(typePath, ancestor, typeDecls, _), body) /* if !typeDecls.isEmpty */ ⇒
      val ancType = ancestor.toType()
      val child = GenericType(
        typePath.name,
        ancestor.toType(),
        body
      )
      ancType.children.foreach { ancChild ⇒ child.addChild(ancChild.duplicate()) }
      parent.addChild(child)
      buildTree(body, child)
    case _ ⇒
  }

  private def checkAncestor(
    node: TypeTrait
  ): Result = {
    val anc = node.ancestor
    if (node.isInstanceOf[BuiltInTypeTrait] && anc == null) Right(Unit)
    else node.parent.get find (_.name == anc.name) match {
      case Some(_) ⇒ Right(Unit)
      case None    ⇒ Left(ValidatorError(s"Type $anc is unknown"))
    }
  }

  /*
   * Second pass: Define generic types and validate parameters and bodies
   */
  private def defineGenericTypes(
    ops: Seq[AST],
    parent: TypeTrait
  ): Unit = ops foreach {
    case MutableTree(TypeDefinition(typePath, _, typeDecls, params), body) ⇒
      val child = typePath.getType(Some(parent)).map { child ⇒
        val validator = ExpressionValidator(child)
        validator.validate(typeDecls)
        validator.validate(params)
        child
      }.getOrElse(throw ValidatorError(s"Type $typePath is unknown which should not happen here"))

      defineGenericTypes(body, child)
    case _ ⇒
  }

  private def createGenericTypeImplementation(
    child: GenericTypeTrait,
    typeDecls: Seq[TypePath],
    typeVars: Set[VariableLike],
    body: Seq[AST],
    parent: TypeTrait
  ) = {
    // Copy the generic type only if the implementation happens under a
    // different parent
    val newChild = child.parent.collectFirst {
      case childsParent if childsParent != parent ⇒ parent.addChild(child.duplicate().asInstanceOf[GenericTypeTrait])
    }.getOrElse(child)

    val typeParams = ExpressionValidator.validateGenericTypeDeclarations(newChild, typeDecls)

    // Add generic parameters to generic implementations
    val genericParams = GenericParams(typeParams.toSeq: _*)
    newChild.addImplementation(genericParams, typeVars, body)
  }

  private def implementAbstractType(
    child: AbstractTypeTrait,
    typeVars: Set[VariableLike],
    body: Seq[AST],
    parent: TypeTrait
  ) = {
    println(s"Validator:checkTypeOperations! Adding: $child to: $parent")
    // Here parent should have this abstract type
    /* TODO: Check if parent contains `abstType` */
    val newType: UserTypeTrait = {
      val fullBody = child match {
        case userType: UserTypeTrait ⇒ userType.body ++ body
        case _                       ⇒ body
      }

      if (ExpressionValidator.validateDeclares(fullBody) == fullBody) {
        child match {
          case genericType: GenericUserTypeTrait ⇒
            GenericType(genericType.name, genericType.ancestor, fullBody, genericType.genericBounds, typeVars)
          // Check if it's based on a built-in but non-abstract type
          case builtInType: BuiltInTypeTrait ⇒
            ExtendedBuiltInType(builtInType.name, builtInType.ancestor, fullBody, typeVars)
          case otherType ⇒
            Type(otherType.name, otherType.ancestor, fullBody, typeVars)
        }
      } else {
        child match {
          // Check if it's based on a built-in but non-abstract type
          case builtInType: BuiltInTypeTrait ⇒
            AbstractType(builtInType.name, builtInType.ancestor, fullBody, typeVars)
          case genericType: GenericUserTypeTrait ⇒
            AbstractGenericType(genericType.name, genericType.ancestor, fullBody, genericType.genericBounds, typeVars)
        }
      }
    }

    //Types.copy(abstType, body, variables = vars)
    //println(s"Validator:checkTypeOperations! New type: $newType")
    child.replaceNode(newType)
    //checkTypeOperations(body, parent.find(_.name == newChild.name).get.replaceNode(newChild))
  }

  private def checkTypeOperations(
    ops: Seq[AST],
    parent: TypeTrait
  ): Unit = ops foreach {
    case BodyAST(TypeReopening(typePath, typeDecls, params), body) ⇒
      println(s"Validator:checkTypeOperations! TypeReopening: $typePath; Parent: $parent")
      val absoluteTypeName = typePath.resolve(parent.toPath).asInstanceOf[TypePath]
      absoluteTypeName.getType(Some(parent)).map { child ⇒

        // Convert `params` to Variables
        val vars: Set[VariableLike] = params.flatMap {
          case ASTVariable(name, Some(varType)) ⇒ Some(Variable(name, varType.toType()))
          // TODO: Review! Probably better throw
          case ASTVariable(name, None)          ⇒ None
        }.toSet

        // Check if the parameters match
        if (!child.variables.subsetOf(vars))
          throw ValidatorError(s"Not all parameters from the type definition are included: $vars")

        checkTypeOperations(
          body,
          child match {
            case child: GenericTypeTrait  ⇒ createGenericTypeImplementation(child, typeDecls, vars, body, parent).get
            case child: AbstractTypeTrait ⇒ implementAbstractType(child, vars, body, parent)
            case child: UserTypeTrait ⇒
              child.body ++= body
              child
          }
        )
      }.orElse {
        //Types.root.printTree(true, 0, true)
        throw ValidatorError(s"Type not found: $absoluteTypeName")
      }
    case BodyAST(TypeDefinition(typePath, _, _, _), body) ⇒
      typePath.getType(Some(parent))
        .map(checkTypeOperations(body, _))
        .orElse(throw ValidatorError(s"Type $typePath is unknown which should not happen here"))
    case _ ⇒
  }

}
