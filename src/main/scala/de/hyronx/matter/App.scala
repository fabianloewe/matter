package de.hyronx.matter

import java.io.File
import java.nio.file.Paths

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

import de.hyronx.matter.compiler._

object App extends scala.App {
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

  val parser = new scopt.OptionParser[Config]("matter") {
    head("matter", "0.1")

    cmd("new").action({ (_, cfg) ⇒ cfg.copy(op = NewOp) })
      .text("Initializes a new Matter project")
      .children(
        opt[File]('d', "dir").optional
          .valueName("<dir>")
          .action({ (dir, cfg) ⇒ cfg.copy(outDir = dir) }),
        arg[String]("<project-name>").unbounded
          .action({ (name, cfg) ⇒ cfg.copy(packageName = name) })
      )

    cmd("compile").action({ (_, cfg) ⇒ cfg.copy(op = CompileOp) })
      .text("compile is a command")
      .children(
        opt[String]("package").optional
          .text("Overwrites the package name")
          .valueName("<capitalized-name>")
          .action({ (name, cfg) ⇒ cfg.copy(packageName = name) }),
        opt[File]("build-dir").optional
          .valueName("<dir>")
          .action({ (dir, cfg) ⇒ cfg.copy(buildDir = dir) }),
        arg[File]("<file>...").unbounded
          .action({ (file, cfg) ⇒ cfg.copy(files = cfg.files :+ file) })
      )
  }

  parser.parse(args, Config()) match {
    case Some(cfg) ⇒ cfg.op match {
      case NewOp ⇒
        val projectPath = Paths.get(cfg.outDir.toString, cfg.packageName)
        projectPath.toFile.mkdirs
      case CompileOp ⇒
        // This config gets passed down to e.g. ParserGenerator
        implicit val config = cfg
        config.buildDir.mkdirs

        config.files foreach { file ⇒
          try {
            Parser(Source.fromFile(file).mkString) match {
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
    case None ⇒
      println("The command line arguments are not correctly provided")
  }

  /*
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
  }*/
}
