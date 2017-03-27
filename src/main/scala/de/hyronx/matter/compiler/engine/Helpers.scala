package de.hyronx.matter.compiler.generators

import scala.reflect._

import de.hyronx.matter.compiler.types._

object Helpers {
  private def convertClassName[T: ClassTag](f: String ⇒ String): String = {
    classTag[T].runtimeClass.getName match {
      case "void"   ⇒ "V"
      case "int"    ⇒ "I"
      case "bool"   ⇒ "Z"
      case "byte"   ⇒ "B"
      case "float"  ⇒ "F"
      case "double" ⇒ "D"
      case "long"   ⇒ "J"
      case "short"  ⇒ "S"
      case "char"   ⇒ "C"
      case aClass   ⇒ f(aClass.replace('.', '/'))
    }
  }

  def typeToJavaType(matterType: Type) = matterType match {
    case StringType ⇒ "Ljava/lang/String;"
    // Integers are encoded as Java long
    case IntType    ⇒ "J"
    case VoidType   ⇒ "V"
    case BoolType   ⇒ "Z"
    case FloatType  ⇒ "F"

  }

  def capitalize(string: String) = string.substring(0, 1).toUpperCase + string.substring(1)

  object GetClass {
    def apply[T: ClassTag]: String = convertClassName[T] { x ⇒ "L" + x + ";" }
  }

  object GetClassName {
    def apply[T: ClassTag]: String = convertClassName[T] { x ⇒ x }
  }

  object MethodSig1 {
    def apply[Arg1: ClassTag, Ret: ClassTag] = "(" + GetClass[Arg1] + ")" + GetClass[Ret]
  }

  object MethodSig2 {
    def apply[Arg1: ClassTag, Arg2: ClassTag, Ret: ClassTag] = {
      "(" + GetClass[Arg1] + GetClass[Arg2] + ")" + GetClass[Ret]
    }
  }
}
