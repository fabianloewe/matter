package de.hyronx.matter.compiler

import de.hyronx.matter.compiler.errors.TypeError

package object ast {

  /* Implicits */

  implicit class SyntaxASTToTypeConverter(ast: AST) {

    import de.hyronx.matter.compiler.ast._
    import de.hyronx.matter.compiler.types._

    def getType(typ: TypeTrait): TypeTrait = {
      import GenericParams._

      lazy val stringType = Types.matter.collectFirst { case t: TypeTrait if t.name == "String" ⇒ t }.get

      def getGenericType(
        genericName: String,
        setGenerics: GenericParams
      ): GenericTypeImpl = {
        val foundType: scala.Option[GenericTypeTrait] = typ.root collectFirst {
          case x @ GenericTypeTrait(name, _, _, _, _, _, _) if name == genericName ⇒ x
        }

        val result = for {
          genType ← foundType
          impl ← genType.getImplementation(setGenerics)
        } yield impl
        result getOrElse (throw TypeError(s"Cound not find generic type: $genericName"))

        /*
       collect {
        case genType if genType.isGeneric => genType.toGeneric
      } map { genType =>
          genType.getLabel.getImplementation(setGenerics)
        case None ⇒ throw TypeError(s"Cound not find generic type: $genericName")
      }
      */
      }

      def matchSyntax(ast: AST): TypeTrait = ast match {
        case Option(defs) ⇒
          getGenericType("Option", GenericParams("T" → defs.getType(typ)))
        case Repeat(defs) ⇒
          getGenericType("List", GenericParams("T" → defs.getType(typ)))
        case RepeatOne(defs) ⇒
          getGenericType("List", GenericParams("T" → defs.getType(typ)))
        case Literal(string) ⇒
          stringType
        case Range(from, to) ⇒
          stringType
        case BodyAST(_: Concatenation, defs) ⇒
          getGenericType("Tuple", GenericParams("T" → matchTupleTypes(defs)))
        /*case VariableUsage(varName) ⇒ typ.rootLabel.variables find (_.name == varName) match {
          case Some(defs) ⇒ getGenericType("Grammar", Seq(defs.getType(ast)))
          case None       ⇒ throw new ParserError(s"No such variable: $varName")
        } TODO: Implement */
        case other ⇒
          getGenericType("Grammar", GenericParams("T" → Types.matter))
      }

      def matchTupleTypes(ast: Seq[AST]): List[TypeTrait] = ast.headOption match {
        case Some(head) ⇒ matchSyntax(head) :: matchTupleTypes(ast.tail)
        case None       ⇒ List()
      }

      ast match {
        // Selection would not return a type but a generic parameter
        // so it's "special"
        case BodyAST(_: Selection, defs) ⇒
          val types = defs map (_.getType(typ))
          getGenericType("Grammar", {
            if (types forall (_ == types.head)) GenericParams("T" → types.head)
            else GenericParams("T" → types)
          })
        // TypeName results in its corresponding type thus should not be wrapped
        // in the Grammar type
        /*
        case typeName: TypeName ⇒
          typ.find(_ == typeName)
            .orElse(Types.matterType.find(_ == typeName))
            .getOrElse(throw TypeError(s"Could not find type: $typeName"))
            */
        case _ ⇒
          getGenericType("Grammar", GenericParams("T" → matchSyntax(ast)))
      }
    }
  }

}
