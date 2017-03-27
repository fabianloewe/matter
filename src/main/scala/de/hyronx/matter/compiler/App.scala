package de.hyronx.matter.compiler

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object CompilerApp extends App {
  def printMatterTypes(matterType: ast.MatterTypeTree, indent: Int = 0): Unit = {
    println(" " * indent + s"${matterType.id}:")
    println(" " * indent + s"  HashCode: ${matterType.hashCode}")
    println(" " * indent + s"  Ancestor: ${matterType.ancestor.id}")
    println(" " * indent + s"  Descendants: ${matterType.descendants.map(_.id)}")
    println(" " * indent + s"  Parent: ${matterType.parent.id}")
    println(" " * indent + s"  Children: ${matterType.children.map(_.id)}")

    if (matterType.isInstanceOf[ast.MatterType]) {
      val matter = matterType.asInstanceOf[ast.MatterType]
      println(" " * indent + s"  Syntax: ${matter.syntax}")
      println(" " * indent + s"  Mapping: ${matter.mappings}")
      println(" " * indent + s"  Is abstract: ${matter.isAbstract}")
    }

    matterType.children foreach { child ⇒
      printMatterTypes(child, indent + 2)
    }
  }

  if (args.length == 0)
    println("Please provide some arguments!")

  for (arg ← args) {
    try {
      val code = Source.fromFile(arg).mkString
      Parser(code) match {
        case Left(ParserError(msg)) ⇒ println("Parser failed: " + msg)
        case Right(result) ⇒
          printMatterTypes(result)
          Generator(result)
      }
    } catch {
      case e: java.io.FileNotFoundException ⇒ println(e.getMessage)
    }
  }
}
