package de.hyronx.matter

import java.io.File
import java.nio.file.{Path, Paths}

import scala.io.Source
import de.hyronx.matter.management._
import de.hyronx.matter.compiler._
import de.hyronx.matter.compiler.errors.{CompilationError, ParserError, ValidatorError}

object App extends scala.App {
  val parser = new scopt.OptionParser[Config]("matter") {
    head("matter", BuildInfo.version)

    cmd("new")
      .text("Initializes a new Matter project.")
      .children(
        opt[File]('d', "dir")
          .optional
          .text("Creates the project in a different directory")
          .valueName("<dir>")
          .action({ (dir, config) ⇒
            config.copy(
              outDir = dir,
              buildDir = dir.toPath.resolve(config.buildDir.toPath).toFile
            )
          }),
        opt[String]('v', "vendor")
          .optional
          .text("Initialize with vendor")
          .valueName("<name>")
          .action({ (name, config) ⇒ config.copy(vendor = Some(name)) }),
        opt[String]('i', "initial-version")
          .optional
          .text("Initialize with version")
          .valueName("<version>")
          .action({ (version, config) ⇒ config.copy(version = version) }),
        opt[Unit]("no-git")
          .optional
          .text("Do not use Git for versioning")
          .action({ (_, config) ⇒ config.copy(useGit = false) }),
        arg[String]("<project-name>")
          .unbounded
          .text("Project to be created")
          .action({ (name, config) ⇒ config.copy(packageName = name) })
      ).action({ (_, config) ⇒ config.copy(op = NewOp) })

    cmd("compile")
      .text("Compiles the current project or specified files.")
      .children(
        opt[File]('d', "dir")
          .optional
          .text("Compiles a project in a different directory")
          .valueName("<dir>")
          .action({ (dir, config) ⇒
            config.copy(
              outDir = dir,
              buildDir = dir.toPath.resolve(config.buildDir.toPath).toFile,
              packageName = dir.toPath.getFileName.toString
            )
          }),
        opt[String]('p', "package")
          .optional
          .text("Overwrites the package name")
          .valueName("<capitalized-name>")
          .action({ (name, config) ⇒ config.copy(packageName = name) }),
        opt[File]('o', "build-dir")
          .optional
          .text("Uses a different build directory")
          .valueName("<dir>")
          .action({ (dir, config) ⇒ config.copy(buildDir = dir) }),
        opt[Unit]("no-git")
          .optional
          .text("Do not use Git for versioning")
          .action({ (_, config) ⇒ config.copy(useGit = false) }),
        arg[File]("<file>...")
          .optional
          .unbounded
          .text("Files to be compiled")
          .action({ (file, config) ⇒ config.copy(files = config.files :+ file) })
      ).action({ (_, config) ⇒ config.copy(op = CompileOp) })

    cmd("run")
      .text("Runs the current project.")
      .children(
        opt[File]('d', "dir")
          .optional
          .text("Searches for a project in a different directory")
          .valueName("<dir>")
          .action({ (dir, config) ⇒
            config.copy(
              outDir = dir,
              buildDir = dir.toPath.resolve(config.buildDir.toPath).toFile,
              packageName = dir.toPath.getFileName.toString
            )
          }),
        opt[String]('p', "package")
          .optional
          .text("Overwrites the package name")
          .valueName("<capitalized-name>")
          .action({ (name, config) ⇒ config.copy(packageName = name) }),
        opt[File]('o', "build-dir")
          .optional
          .text("Uses a different build directory")
          .valueName("<dir>")
          .action({ (dir, config) ⇒ config.copy(buildDir = dir) }),
        opt[Unit]("no-git")
          .optional
          .text("Do not use Git for versioning")
          .action({ (_, config) ⇒ config.copy(useGit = false) }),
        arg[File]("<file>...")
          .optional
          .unbounded
          .text("Files to be passed to the executed program")
          .action({ (file, config) ⇒ config.copy(files = config.files :+ file) })
      ).action({ (_, config) ⇒ config.copy(op = RunOp) })

    opt[Unit]("regenerate")
      .optional
      .text("Regenerate the Matter built-in types")
      .action({ (_, config) ⇒ config.copy(regenerate = false) })
    ,
  }

  parser.parse(args, Config()) match {
    case Some(cfg) ⇒ cfg.op match {
      case NewOp ⇒
        // This config gets passed down to e.g. Project
        implicit val config: Config = cfg

        val projectPath = Paths.get(config.outDir.toString, config.packageName)
        Project.create(projectPath)
      case CompileOp ⇒
        // This config gets passed down to e.g. ParserGenerator
        implicit val config: Config = cfg

        def compileSources(
                            tool: BuildTool,
                            projectPath: Path = Paths.get("."),
                            srcs: Seq[File] = Seq(),
                            projectName: Option[String] = None
                          )(implicit config: Config) = {
          import sys.process._

          println(s"App:compileSources! Project path: ${projectPath.normalize().toAbsolutePath}")

          tool match {
            case BuildTool.Sbt ⇒ Seq("/bin/sh", "-c", s"cd ${projectPath.normalize().toAbsolutePath}; sbt compile").!
            case BuildTool.Matter ⇒ srcs.foreach { file ⇒
              try {
                Parser(Source.fromFile(file).mkString).fold(
                  { e: ParserError ⇒ ErrorHandler(e, "Parsing", Some(file.toString)) },
                  { result ⇒
                    //import sext._

                    //println(result.treeString)
                    Validator(result, projectName) match {
                      case Left(e: ValidatorError) ⇒ ErrorHandler(e, "Validation")
                      case Left(e: Exception) ⇒ ErrorHandler(e, "Validation")
                      case Right(r) ⇒ Generator(r)
                    }
                    //Generator(result)
                  })
              } catch {
                case e: java.io.FileNotFoundException ⇒ ErrorHandler(e, "Preparation")
                case e: CompilationError ⇒ ErrorHandler(e, "Compilation")
              }
            }
          }
        }

        if (config.files.isEmpty) {
          // Open the project
          val projectDir = config.outDir.toPath
          Project.open(projectDir.normalize().toAbsolutePath()) fold( { error ⇒ ErrorHandler(error, "Project configuration", Some("config.yaml")) }, { proj ⇒
            implicit val project = proj
            // Create the build directory in project directory
            projectDir.resolve(config.buildDir.toPath).toFile.mkdirs

            // Get source files to compile
            val sources = project.sources
            if (sources.isEmpty)
              ErrorHandler("No sources specified", "Command execution")

            // Compile supported source files
            sources foreach {
              case (Language.Scala, _) ⇒ compileSources(BuildTool.Sbt, projectDir)
              case (Language.Java, _) ⇒ compileSources(BuildTool.Sbt, projectDir)
              case (Language.Matter, files) ⇒ compileSources(
                BuildTool.Matter,
                srcs = sources(Language.Matter),
                projectName = Some(project.name))
            }
          }
          )
        } else {
          compileSources(
            BuildTool.Matter,
            srcs = config.files,
            projectName = Some(cfg.packageName))
        }
      case RunOp ⇒
        import sys.process._

        val config = cfg
        val compilerPath = java.lang.System.getProperty("user.dir")

        println(s"Compiler path: $compilerPath")
        val result = s"scala -cp $compilerPath/target/universal/stage/lib/com.lihaoyi.fastparse_2.12-0.4.2.jar:" +
          s"$compilerPath/target/universal/stage/lib/com.lihaoyi.fastparse-utils_2.12-0.4.2.jar:" +
          s"$compilerPath/target/universal/stage/lib/com.lihaoyi.sourcecode_2.12-0.1.3.jar:" +
          s"$compilerPath/target/universal/stage/lib/de.hyronx.matter-0.0.1.jar:" +
          s"${config.buildDir.toPath.toAbsolutePath()} " +
          s"${config.packageName}.Compiler " +
          s"${config.files.mkString(" ")}"

        println(s"Result: ${result.!}")
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
