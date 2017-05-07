package de.hyronx.matter.compiler

import scala.collection.mutable.ListBuffer

import cafebabe.Package
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._

import de.hyronx.matter.Config
import de.hyronx.matter.compiler.ast.{ MatterTypeTree, MatterType }
import de.hyronx.matter.compiler.generators._

object Generator {
  def generate(matterType: MatterType)(implicit config: Config, pkg: Package) = {
    println(s"Generator:generate! Searching for $matterType")
    if (matterType.isAbstract)
      throw new GeneratorError(s"Matter type ${matterType.id} is abstract and cannot be generated.")

    pkg.findClass(matterType.id) match {
      case Some(foundClass) ⇒
        println("Generator:generate! Found!")
        foundClass
      case None ⇒
        println("Generator:generate! Not found!!!")
        ParserGenerator(matterType, ClassGenerator(matterType))
    }
  }

  def apply(base: MatterTypeTree)(implicit config: Config) = {
    implicit val pkg = new Package(config.packageName)

    base.children
      .collect { case child: MatterType if !child.isAbstract ⇒ child }
      .foreach(generate(_))

    val mainClass = pkg.addClass("Compiler")
    mainClass.addDefaultConstructor
    val mainMethodCH = mainClass.addMainMethod.codeHandler
    val stringVar = mainMethodCH.getFreshVar("Ljava/lang/String;")
    val parsedVar = mainMethodCH.getFreshVar("Lfastparse/core/Parsed;")

    /* The corresponding code in Java:
      public static void main(String[] args) {
        String[] string = new String(Files.readAllBytes(Paths.get(args[0])));
        Parser<?> parser = NumberAssignment.parse();
        System.out.println(parser.parse().get().value().toString
      }
    */
    mainMethodCH <<
      New("java/lang/String") <<
      DUP <<
      ArgLoad(0) <<
      Ldc(0) <<
      AALOAD <<
      Ldc(0) <<
      NewArray("java/lang/String") <<
      InvokeStatic("java/nio/file/Paths", "get", "(Ljava/lang/String;[Ljava/lang/String;)Ljava/nio/file/Path;") <<
      InvokeStatic("java/nio/file/Files", "readAllBytes", "(Ljava/nio/file/Path;)[B") <<
      InvokeSpecial("java/lang/String", "<init>", "([B)V") <<
      AStore(stringVar) <<
      InvokeStatic(s"${pkg.javaName}/NumberAssignment", "parse", "()Lfastparse/core/Parser;") <<
      ALoad(stringVar) <<
      Ldc(0) <<
      ACONST_NULL <<
      InvokeVirtual("fastparse/core/Parser", "parse", "(Ljava/lang/Object;ILscala/Function3;)Lfastparse/core/Parsed;") <<
      AStore(parsedVar) <<
      GetStatic("java/lang/System", "out", "Ljava/io/PrintStream;") <<
      ALoad(parsedVar) <<
      InvokeInterface("fastparse/core/Parsed", "get", "()Lfastparse/core/Parsed$Success;") <<
      InvokeVirtual("fastparse/core/Parsed$Success", "value", "()Ljava/lang/Object;") <<
      InvokeVirtual("java/lang/Object", "toString", "()Ljava/lang/String;") <<
      InvokeVirtual("java/io/PrintStream", "println", "(Ljava/lang/String;)V") <<
      RETURN
    mainMethodCH.freeze

    pkg.writeTo(config.buildDir.toString)
  }
}
