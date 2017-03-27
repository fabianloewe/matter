package de.hyronx.matter.compiler

import scala.collection.mutable.ListBuffer

import de.hyronx.matter.compiler.ast.{ MatterTypeTree, MatterType }
import de.hyronx.matter.compiler.generators._

object Generator {
  private val generatedClasses = ListBuffer.empty[cafebabe.ClassFile]

  def generate(matterType: MatterType) = {
    println(s"Generator:generate! Searching for $matterType")
    if (matterType.isAbstract)
      throw new java.lang.RuntimeException(s"Matter type ${matterType.id} is abstract and cannot be generated.")

    generatedClasses find (_.className == matterType.id) match {
      case Some(foundClass) ⇒
        println("Generator:generate! Found!")
        foundClass
      case None ⇒
        println("Generator:generate! Not found!!!")
        val classFile = ParserGenerator(matterType, ClassGenerator(matterType))
        generatedClasses += classFile
        classFile
    }
  }

  def apply(base: MatterTypeTree) = {
    base.children
      .collect { case child: MatterType if !child.isAbstract ⇒ child }
      .foreach { child ⇒
        println(s"Generator! Child: ${child}")

        val classFile = generate(child)
        classFile.writeToFile(child.id + ".class")
      }
  }
}
