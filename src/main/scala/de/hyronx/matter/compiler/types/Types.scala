package de.hyronx.matter.compiler.types

import de.hyronx.matter.compiler.errors.TypeError
import de.hyronx.matter.library.MutableTree

object Types {

  import GenericParams._

  val matter: TypeTrait = TypeTrait(
    BuiltInType("Matter"),

    /*
      * Abstract types to be implemented by the user
      *
      */
    TypeTrait(
      AbstractBuiltInType("Syntax")
    ),
    TypeTrait(
      AbstractBuiltInType("Mapping")
    ),
    TypeTrait(
      AbstractBuiltInType("Compilation")
    )
  )

  /*
    * Base types
    * TODO: Add functionality (through variables)
    *
    */
  val builtIn = BuiltInType(
    "BuiltIn",
    anc = Some(matter)
  )
  val union = GenericBuiltInType(
    "Union",
    GenericParams("T" → Set(matter)),
    anc = Some(builtIn)
  )

  /*
    * Colllection types
    *
    */
  val collection = GenericBuiltInType(
    "Collection",
    GenericParams("T" → matter),
    anc = Some(builtIn)
  )
  val string = PrimitiveBuiltInType(
    "String",
    "String",
    anc = Some(collection)
  )
  val option = GenericBuiltInType(
    "Option",
    GenericParams("T" → matter),
    anc = Some(collection)
  )
  val list = GenericBuiltInType(
    "List",
    GenericParams("T" → matter),
    anc = Some(collection)
  )
  val tuple = GenericBuiltInType(
    "Tuple",
    GenericParams("T" → Seq(matter)),
    anc = Some(collection),
    implementationInit = { (genImpl) ⇒
      genImpl.variables = genImpl.variables ++ genImpl.genericBounds.collectFirst {
        case VariadicGenericParameter(_, types) ⇒ types.zipWithIndex.map {
          case (someType, index) ⇒ IndexAccessor(index, someType, genImpl)
        }
      }.get
      genImpl
    }
  )

  /*
    * Numeral types
    *
    */

  val numeral = PrimitiveBuiltInType(
    "Numeral",
    "Number",
    anc = Some(builtIn)
  )
  val integer = PrimitiveBuiltInType(
    "Integer",
    "Long",
    anc = Some(numeral)
  )
  val float = PrimitiveBuiltInType(
    "Float",
    "Float",
    anc = Some(numeral)
  )

  /*
      * Implicit types
      * TODO: Add functionality (through variables) where needed
      *
      */
  val void = BuiltInType("Void")
  val boolean = PrimitiveBuiltInType(
    "Boolean",
    "Boolean"
  )
  val grammar = GenericBuiltInType(
    "Grammar",
    GenericParams("T" → matter)
  )

  //println("Printing Matter tree...")
  //matter.printTree(true, 0, true)
  //println("Done")

  val root = TypeTrait(
    BuiltInType("Base"),
    matter,

    builtIn,
    collection,
    numeral,

    string,
    option,
    list,
    tuple,
    integer,
    float,

    void,
    boolean,
    grammar
  )

}
