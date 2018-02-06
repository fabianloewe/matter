package de.hyronx.matter.compiler

import scala.collection.mutable.LinkedHashSet

import scalaz.{ Tree, TreeLoc }

import de.hyronx.matter.compiler.ast.AST

package object types {
  type TypeTraitNode = TypeTrait
  type TypeNode = Type
  type GenericTypeNode = GenericTypeTrait
  type BuiltInNode = BuiltInTypeTrait
  type UserTypeNode = UserTypeTrait

  implicit def defaultMemberValidator(typ: TypeTrait, member: String): Boolean = typ.variables.exists(_.name == member)

  /* Generics */

  type GenericParam = GenericParameterTrait
  type GenericParams = LinkedHashSet[GenericParameterTrait]

  object GenericParams {
    implicit def tuple2UGP(
      tuple: (String, Set[TypeTraitNode])
    ): UnionGenericParameter = UnionGenericParameter(tuple._1, tuple._2)

    implicit def tuple2SGP(
      tuple: (String, TypeTraitNode)
    ): SingleGenericParameter = SingleGenericParameter(tuple._1, tuple._2)

    implicit def tuple2VGP(
      tuple: (String, Seq[TypeTraitNode])
    ): VariadicGenericParameter = VariadicGenericParameter(tuple._1, tuple._2)

    implicit val ordering = new Ordering[GenericParam]() {
      def compare(a: GenericParam, b: GenericParam) = a.name.compareTo(b.name)
    }

    def apply(set: GenericParameterTrait*): GenericParams = LinkedHashSet(set: _*)
  }

  /* Implicits */
  /*
  implicit class TypeTraitEnhancer[+T <: TypeTrait](typ: TreeLoc[T]) {
    import de.hyronx.matter.compiler.ast.TypeName

    val toJavaClass = typ.path.mkString("/")

    val toJavaType = s"L$toJavaClass;"

    def isUserType() = typ.getLabel.isInstanceOf[UserTypeTrait]
    def asUserType = typ.asInstanceOf[TreeLoc[UserTypeTrait]]

    def isGeneric() = typ.getLabel.isInstanceOf[GenericTypeTrait]
    def asGeneric = typ.asInstanceOf[TreeLoc[GenericTypeTrait]]

    def isAbstract() = typ.getLabel.isInstanceOf[AbstractTypeTrait]
    def isBuiltInType() = typ.getLabel.isInstanceOf[BuiltInTypeTrait]
    def hasAncestor() = !(typ.isBuiltInType && typ.getLabel.ancestor == null)


    def implementations: Set[TreeLoc[GenericTypeImpl]] = {
      if (typ.isGeneric) typ.asGeneric.getLabel.implementations
      else Set()
    }


    def addImplementation(
      bounds: GenericParams,
      variables: Set[VariableLike],
      body: Seq[AST]
    ): Option[TreeLoc[GenericTypeImpl]] = if (typ.isGeneric) {
      val genType = typ.asGeneric.getLabel
      //genType.implementations find (_.bounds ==)
      /* TODO: Recheck if all casts are necessary and safe */
      val genImpl = typ.copy()
        .asInstanceOf[TypeTraitNode]
        .setLabel(GenericTypeImpl(
          typ.asGeneric,
          genType.name + bounds.map {
            case SingleGenericParameter(_, single: TypeTraitNode) ⇒
              "$$" + single.getLabel.name
            case UnionGenericParameter(_, multiple: Set[TypeTraitNode]) ⇒
              "$$" + multiple.map(_.getLabel.name).mkString("$") + "$$"
            case VariadicGenericParameter(_, multiple: Seq[TypeTraitNode]) ⇒
              "$$" + multiple.map(_.getLabel.name).mkString("$") + "$$"
          }.mkString("$"),
          genType.ancestor,
          bounds,
          variables,
          if (typ.isUserType) typ.asUserType.getLabel.body ++ body else body
        ))
        .asInstanceOf[TreeLoc[GenericTypeImpl]]

      genType.implementations = genType.implementations + genImpl
      Some(genImpl)
    } else {
      None
    }

    def getImplementation(genParams: GenericParams): Option[TreeLoc[GenericTypeImpl]] = {
      if (typ.isGeneric) {
        typ.asGeneric.getLabel.implementations.find(_.getLabel.genericBounds == genParams)
      } else {
        None
      }
    }

    def getImplementation(genParams: Seq[TypeTraitNode]): Option[TreeLoc[GenericTypeImpl]] = {
      println(s"TypeTraitEnhancer:getImplementation! Type: ${typ.getLabel.name}")
      if (typ.isGeneric) {
        val genType = typ.asGeneric
        genType.getLabel.implementations find (_.getLabel.genericBounds.map {
          case SingleGenericParameter(_, genType) ⇒ Some(genType)
          case _                                  ⇒ None
        }.flatten.toSeq == genParams) orElse {
          import GenericParams._

          val collector: PartialFunction[(GenericParameterTrait, TypeTraitNode), SingleGenericParameter] = {
            case (GenericParameterTrait(name), node: TypeTraitNode) ⇒
              SingleGenericParameter(name, node)
          }

          val bounds: GenericParams = SortedSet(
            (genType.getLabel.genericBounds, genParams).zipped
              .collect(collector).toSeq: _*
          )

          genType.addImplementation(
            bounds,
            typ.getLabel.variables,
            if (genType.isUserType) genType.asUserType.getLabel.body
            else Seq()
          )
        }
      } else {
        None
      }
    }

    //@scala.annotation.tailrec
    final def isAncestor(that: TypeTraitNode): Boolean = {
      if (typ.getLabel.ancestor isEqual that)
        true
      else if (typ.getLabel.ancestor.hasAncestor)
        typ.getLabel.ancestor.isAncestor(that)
      else
        false
    }

    def getTypeName: TypeName = TypeName(
      typ.getLabel.name,
      typ.parents.map {
      case (_, parent, _) ⇒ parent.name
    }.reverse.toSeq
    )

    def findChild(
      typeName: TypeName
    ): Option[TypeTraitNode] = typ.asInstanceOf[TypeTraitNode]
      .find(_ isEqual typeName)
      .orElse {
        var result: Option[TypeTraitNode] = None
        if (typ.hasAncestor) {
          println(s"TypeTraitEnhancer:findChild! Type ${typ.getTypeName} has an ancestor: ${typ.getLabel.ancestor.getTypeName}")
          result = typ.getLabel.ancestor.findChild(typeName)
        }

        val resultString = result map { res ⇒ res.getTypeName.toString }
        println(s"TypeTraitEnhancer:findChild! Result: $resultString")
        result orElse {
          val upper = typeName.upper
          if (upper == typeName) None
          else typ.asInstanceOf[TypeTraitNode] findChild upper
        }
      }

    def isEqual(that: Any): Boolean = that match {
      case that: TypeName ⇒
        if (that.family.isEmpty) that.name == typ.getLabel.name
        else {
          /*
          val fullPath = typ.path.reverse.tail.map(_.name).toSeq
          println(s"TypeTraitEnhancer:isEqual! TypeName as path: $fullPath")
          (that.family :+ that.name) == fullPath
          */
          that == typ.getTypeName
        }
      case that: String ⇒ that == typ.getLabel.name
      case that: TreeLoc[T] ⇒ that.getLabel.name == typ.getLabel.name &&
        that.path == typ.path
    }

    def addChild(child: Tree[TypeTrait]): TypeTraitNode = {
      def setImplementation(
        thisType: GenericTypeNode,
        currentImpl: TreeLoc[GenericTypeImpl],
        newImpl: TreeLoc[GenericTypeImpl]
      ): TypeTraitNode = {
        val impls = thisType.getLabel.implementations - currentImpl
        thisType.getLabel.implementations = impls + newImpl
        thisType.printTree(detailed = true, withSiblings = false)
        thisType.asInstanceOf[TypeTraitNode]
      }

      val typeNode = typ.asInstanceOf[TypeTraitNode]
      val childLoc = child.loc
      typeNode.find(_ isEqual childLoc) match {
        case Some(existingChild) ⇒
          Types.replaceChild(typeNode, existingChild, child)
        case None ⇒
          if (typ.getLabel.isInstanceOf[GenericTypeImpl]) {
            // As GenericTypeImpl are not part of the main tree
            // we need to check them specifically.
            // If a direct child with same name exists, replace it in this
            // generics implementation
            typeNode.findChild(_.rootLabel.name == child.rootLabel.name) match {
              case Some(existingChild) ⇒
                val genTypeImpl = typ.asInstanceOf[TreeLoc[GenericTypeImpl]]

                // Then replace the current known implementation with this new one
                setImplementation(
                  genTypeImpl.getLabel.basedOn,
                  genTypeImpl,
                  existingChild.setTree(child).parent.get.asInstanceOf[TreeLoc[GenericTypeImpl]]
                )
              case None ⇒
                typeNode.insertDownLast(child)
            }
          } else {
            Types.addChildTo(typeNode, child)
          }
      }
    }

    def foreachChild(f: TypeTraitNode ⇒ Unit): Unit = {
      @scala.annotation.tailrec
      def iterChildren(child: TypeTraitNode): Unit = {
        child foreach f
        if (!child.isLast) iterChildren(child.right.get)
      }

      if (typ.hasChildren) iterChildren(typ.firstChild.get.asInstanceOf[TypeTraitNode])
    }

    def children(): Set[TypeTraitNode] = {
      var childrenSet = Set.empty[Option[TypeTraitNode]]
      if (typ.hasChildren) {
        var count = 0
        var child = typ.firstChild
        while (child map (!_.isEqual(typ.lastChild.get)) getOrElse false) {
          childrenSet = childrenSet + child.asInstanceOf[Option[TypeTraitNode]]
          count += 1
          child = typ.getChild(count)
        }
      }
      childrenSet.flatten
    }

    def foreach(f: TypeTraitNode ⇒ Unit): Unit = {
      f(typ.asInstanceOf[TypeTraitNode])
      if (typ.hasChildren) typ.firstChild map (_ foreach f)
      else if (!typ.isLast) typ.right map (_ foreach f)
      else for {
        parent ← typ.parent
        rightSib ← parent.right
      } rightSib foreach f
    }

    def print(
      detailed: Boolean = false,
      indent: Int = 0,
      withSiblings: Boolean = true
    ): Unit = {
      def matchType(
        typ: TypeTrait,
        list: Set[String] = Set()
      ): Set[String] = typ match {
        case _: AbstractTypeTrait if !list.contains("Abstract") ⇒ matchType(typ, list + "Abstract")
        case _: BuiltInTypeTrait if !list.contains("Built-in") ⇒ matchType(typ, list + "Built-in")
        case _: GenericTypeTrait if !list.contains("Generic") ⇒ matchType(typ, list + "Generic")
        case _: UserTypeTrait if !list.contains("Matter-based") ⇒ matchType(typ, list + "Matter-based")
        case _ ⇒ list
      }

      println(" " * indent + typ.getLabel.name)
      if (detailed) {
        println(" " * (indent + 2) + s"Type: ${matchType(typ.getLabel).mkString(", ")}")
        val typeString = typ.getLabel match {
          case genType: GenericTypeTrait ⇒
            println(" " * (indent + 4) + s"Generic bounds: ${genType.genericBounds.map(_.name).mkString}")
            if (!genType.implementations.isEmpty) {
              println(" " * (indent + 4) + s"Generic implementations:")
              genType.implementations.foreach(_.printTree(true, indent + 6, false))
            }
          case _ ⇒
        }

        if (typ.getLabel.ancestor != null)
          println(" " * (indent + 2) + s"Ancestor: ${typ.getLabel.ancestor.getLabel.name}")
        if (!typ.getLabel.variables.isEmpty)
          println(" " * (indent + 2) + s"Variables: ${typ.getLabel.variables.mkString}")
      }
    }

    def printTree(
      detailed: Boolean = false,
      indent: Int = 0,
      withSiblings: Boolean = true
    ): Unit = {
      typ.print(detailed, indent, withSiblings)
      if (typ.hasChildren) {
        println(" " * (indent + 2) + "Children:")
        typ.firstChild map (_.printTree(detailed, indent + 4, withSiblings))
      } else if (!typ.isLast && withSiblings) {
        typ.right map (_.printTree(detailed, indent, withSiblings))
      } else if (withSiblings) {
        for {
          parent ← typ.parent
          rightSib ← parent.right
        } rightSib.printTree(detailed, indent - 4, withSiblings)
      }
    }
  }
  */
}
