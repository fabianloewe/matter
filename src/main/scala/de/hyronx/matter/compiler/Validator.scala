package de.hyronx.matter.compiler

import de.hyronx.matter.compiler.ast._

class Validator(ast: ExpressionList[AST]) {
  import scalaz.{ Tree, TreeLoc }
  import Validator._

  sealed trait AbstractType {
    def name: String
    def ancestor: Tree[AbstractType]
  }
  case class Type(name: String, ancestor: Tree[AbstractType]) extends AbstractType
  case object BaseType extends AbstractType {
    val name = "Base"
    // This is secure as long as the ancestor is never checked in BaseType
    val ancestor = null
  }

  val baseNode = Tree.Node[AbstractType](BaseType, buildTree(ast.ops)).loc
  val matterNode = Tree.Leaf[AbstractType](Type("Matter", baseNode.toTree))

  def validate(): Unit = {
    def walkTree(children: Stream[Tree[AbstractType]]): Unit = children foreach { child ⇒
      checkAncestor(child) match {
        case Right(_)  ⇒ walkTree(child.subForest)
        case Left(err) ⇒ throw err
      }
    }

    walkTree(baseNode.toTree.subForest)
    checkTypeOperations(ast.ops, baseNode)
  }

  /*
   * First pass: Build the type tree
   *
   * Notes:
   * Must be the first check. The following checks may be run asynchronously.
   *
   * This method throws ValidatorError because Either became unhandable
   * through the mappings.
   */
  private def buildTree(
    ops: Seq[AST]
  ): Stream[Tree[AbstractType]] = ops.collect {
    case TypeDefinition(name, ancestor, _, _, body) ⇒
      val ancNode = {
        // If the ancestor is Matter we don't have to search
        if (ancestor.name == "Matter") {
          matterNode
        } else {
          val possibleAnc = baseNode.find { node: TreeLoc[AbstractType] ⇒
            // Check if this node is the ancestor
            if (ancestor.name == node.getLabel.name) {
              // Check if it really is the correct type by
              // traversing its parents
              node.path.map(_.name) == ancestor.family
            } else {
              false
            }
          }

          possibleAnc match {
            case Some(anc) ⇒ anc.toTree
            case None      ⇒ throw ValidatorError(s"Ancestor not found: $ancestor")
          }
        }

      }
      Tree.Node(Type(name.name, ancNode), buildTree(body))
  }.toStream

  private def checkAncestor(
    node: Tree[AbstractType]
  ): Result = {
    val anc = node.rootLabel.ancestor.loc
    // Check if ancestor's parent is a root and has name "Matter"
    if (!anc.parent.map(_.isChild).getOrElse(false) && anc.getLabel.name == "Matter") {
      Right(Unit)
    } else {
      node.loc.parent find (_.getLabel.name == anc.getLabel.name) match {
        case Some(_) ⇒ Right(Unit)
        case None    ⇒ Left(ValidatorError(s"Type ${anc.getLabel.name} is unknown"))
      }
    }
  }

  /*
   * Second pass: Validate type reopenings
   *
   */
  private def checkTypeOperations(
    ops: Seq[AST],
    parent: TreeLoc[AbstractType]
  ): Unit = ops collect {
    case TypeDefinition(name, ancestor, typeDecls, params, _) =>
      parent findChild (_.rootLabel.name == name && _.rootLabel.ancestor == ancestor.name) match {
        case Some(child) => ExpressionValidator(parent, typeDecls)
        case _ => // TODO: Handle? But should never happen
      }
    case TypeReopening(typeName, _, _, body) ⇒
      parent findChild (_.rootLabel.name == typeName.name) match {
        case None if typeName.family.isEmpty ⇒ throw new ValidatorError(s"Type not found: $typeName")
        case None                            ⇒ // TODO: Search
        case _                               ⇒
      }
  }
}

object Validator {
  import scala.util.{ Try, Success, Failure }

  type Result = Either[ValidatorError, Unit]

  def apply(ast: ExpressionList[AST]) = Try(new Validator(ast).validate) match {
    case Success(_)     ⇒ Right(Unit)
    case Failure(error) ⇒ Left(ValidatorError(error.getMessage))
  }
}
