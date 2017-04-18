package de.hyronx.matter.compiler.generators

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._

import de.hyronx.matter.Config

import de.hyronx.matter.compiler.ast.{ MatterType, MatterBuiltIn }
import de.hyronx.matter.compiler.types.Type

class ClassGenerator(matterType: MatterType)(implicit config: Config, pkg: Package) {
  import Helpers._

  /*private val ancestor = matterType.ancestor match {
    case MatterBuiltIn ⇒ None
    case _             ⇒ Some(matterType.ancestor.id)
  }*/
  private val classFile = pkg.addClass(matterType.id, None)

  private def generateGetterMethod(fieldName: String, fieldType: String) = {
    val fieldGetter = "get" + Helpers.capitalize(fieldName)
    val getterCode = classFile.addMethod(fieldType, fieldGetter, "").codeHandler
    getterCode <<
      ArgLoad(0) <<
      GetField(classFile.className, fieldName, fieldType) <<
      ARETURN
    getterCode.freeze

    val scalaGetterCode = classFile.addMethod(fieldType, fieldName, "").codeHandler
    scalaGetterCode <<
      ArgLoad(0) <<
      GetField(classFile.className, fieldName, fieldType) <<
      ARETURN
    scalaGetterCode.freeze
  }

  private def generateSetterMethod(fieldName: String, fieldType: String) = {
    val fieldSetter = "set" + Helpers.capitalize(fieldName)
    val setterCode = classFile.addMethod("V", fieldSetter, fieldType).codeHandler
    setterCode <<
      ArgLoad(0) <<
      ArgLoad(1) <<
      PutField(classFile.className, fieldName, fieldType) <<
      RETURN
    setterCode.freeze

    val scalaSetterCode = classFile.addMethod("V", fieldName + "_$eq", fieldType).codeHandler
    scalaSetterCode <<
      ArgLoad(0) <<
      ArgLoad(1) <<
      PutField(classFile.className, fieldName, fieldType) <<
      RETURN
    scalaSetterCode.freeze
  }

  // Returns the Java field type
  def generateGetter(propName: String, propType: Type) = {
    val fieldName = propName
    val fieldType = typeToJavaType(propType)
    classFile.addField(fieldType, fieldName)
    generateGetterMethod(fieldName, fieldType)
    fieldType
  }

  // Returns the Java field type
  def generateSetter(propName: String, propType: Type) = {
    val fieldName = propName
    val fieldType = typeToJavaType(propType)
    classFile.addField(fieldType, fieldName)
    generateSetterMethod(fieldName, fieldType)
    fieldType
  }

  // Returns the Java field type
  def generateProperty(propName: String, propType: Type) = {
    val fieldName = propName
    val fieldType = typeToJavaType(propType)
    classFile.addField(fieldType, fieldName)

    generateGetterMethod(fieldName, fieldType)
    generateSetterMethod(fieldName, fieldType)
    fieldType
  }

  // Takes a List of property name with its types
  private def generateConstructor(properties: List[(String, String)]) = {
    val constructor = classFile.addConstructor(properties map (_._2)).codeHandler
    constructor <<
      ArgLoad(0) <<
      InvokeSpecial.apply("java/lang/Object", "<init>", "()V")

    var index = 1
    properties foreach {
      case (propName, propType) ⇒
        val setter = "set" + Helpers.capitalize(propName)
        constructor <<
          ArgLoad(0) <<
          ArgLoad(index) <<
          InvokeVirtual(classFile.className, setter, "(" + propType + ")V")
        index += 1
    }

    constructor << RETURN
    constructor.freeze
  }

  def generate: ClassFile = {
    val props: List[(String, String)] = matterType.mappings.map { mapping ⇒
      (mapping.mappedVar, generateProperty(mapping.mappedVar, mapping.varType))
    }.toList

    println(s"ClassGenerator:generate! ClassFile: ${classFile.className}")
    println(s"ClassGenerator:generate! Props: $props")

    generateConstructor(props)
    classFile
  }
}

object ClassGenerator {
  def apply(matterType: MatterType)(implicit config: Config, pkg: Package) =
    new ClassGenerator(matterType).generate
}
