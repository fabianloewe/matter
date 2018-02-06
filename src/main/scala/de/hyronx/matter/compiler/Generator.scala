package de.hyronx.matter.compiler

import cafebabe._
import cafebabe.ByteCodes._
import cafebabe.AbstractByteCodes._
import de.hyronx.matter.Config
import de.hyronx.matter.compiler.errors.GeneratorError
import de.hyronx.matter.compiler.types._
import de.hyronx.matter.compiler.generators._

object Generator {
  def generate(typ: UserTypeTrait)(implicit config: Config, pkg: PackageManager): ClassFile = {
    println(s"Generator:generate! Searching for $typ")
    if (typ.isInstanceOf[AbstractTypeTrait])
      throw new GeneratorError(s"Matter type $typ is abstract and cannot be generated.")

    pkg.findClass(typ.name) match {
      case Some(foundClass) ⇒
        println("Generator:generate! Found!")
        foundClass
      case None ⇒
        println("Generator:generate! Not found!!!")
        ClassGenerator(typ)
    }
  }

  def apply(base: BuiltInNode)(implicit config: Config) = {
    import de.hyronx.matter.compiler.types._

    implicit val pkg = new PackageManager(config.packageName)

    //base.printTree(true)
    base.foreach {
      case child: Type ⇒
        println(s"Generator:apply! Generating: $child")
        generate(child)
      case child: GenericUserTypeTrait ⇒
        child.implementations foreach { genChild ⇒
          generate(genChild)
        }
      case _ ⇒
    }

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
      InvokeStatic(s"${pkg.javaName}/Main", "getParser", "()Lfastparse/core/Parser;") <<
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

    pkg.writeTo(config.buildDir.toPath)
  }
}
