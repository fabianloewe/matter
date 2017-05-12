package de.hyronx.matter.management

import java.io.{ File, FileWriter, FileReader }
import java.nio.file.{ Path, Paths, Files, SimpleFileVisitor, FileVisitResult }
import java.nio.file.attribute.BasicFileAttributes

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.constructor.Constructor
import org.yaml.snakeyaml.representer.Representer
import org.yaml.snakeyaml.nodes.{ Tag, NodeTuple }
import org.yaml.snakeyaml.introspector.Property

import org.eclipse.jgit.api.Git

import de.hyronx.matter.{ Config, BuildInfo }

class Project private (
    project: ProjectConfig,
    val git: Option[Git] = None,
    val matterSources: Seq[File] = Seq()
) {
  val matterVersion = project.matter.version

  val name = project.project.name
  val vendor = project.project.vendor
  val version = project.project.version
  val projectType = project.project.`type` match {
    case "Application" ⇒ Project.Application
    case "Library"     ⇒ Project.Library
    case "Backend"     ⇒ Project.Backend
  }
}

object Project {
  sealed trait Type
  case object Application extends Type
  case object Library extends Type
  case object Backend extends Type

  sealed trait Error
  case class InvalidMatterVersion(thisVersion: String) extends Error {
    override val toString = s"The specified version is incompatible with version $thisVersion"
  }
  case object InvalidType extends Error {
    override val toString = s"The specified type is invalid. Please choose between $Application, $Library and $Backend"
  }

  // Needed to exclude "empty" variables
  private class ProjectRepresenter extends Representer {
    // Leave out null values
    override def representJavaBeanProperty(
      javaBean: Object,
      property: Property,
      propertyValue: Object,
      customTag: Tag
    ): NodeTuple = {
      if (propertyValue == null) {
        null
      } else {
        super.representJavaBeanProperty(javaBean, property, propertyValue, customTag)
      }
    }
  }

  private def createConfig(path: Path, config: Config) = {
    val configFile = Paths.get(path.toString, "config.yaml")
    val fileWriter = new FileWriter(configFile.toFile)

    val projectInfo = ProjectInfo(config.packageName, config.vendor.getOrElse(null), config.version)
    val project = ProjectConfig(project = projectInfo)
    fileWriter.write(new Yaml(new ProjectRepresenter()).dumpAsMap(project))
    fileWriter.flush()

    project
  }

  private def readConfig(path: Path): Either[Error, ProjectConfig] = {
    val configPath = Paths.get(path.toString, "config.yaml")
    val fileReader = new FileReader(configPath.toFile)

    val yaml = new Yaml(new Constructor(classOf[ProjectConfig]))
    val config = yaml.load(fileReader).asInstanceOf[ProjectConfig]

    validateConfig(config)
  }

  private def validateConfig(config: ProjectConfig): Either[Project.Error, ProjectConfig] = {
    if (config.matter.version != BuildInfo.version) {
      Left(InvalidMatterVersion(BuildInfo.version))
    } else {
      config.project.`type` match {
        case "Application" | "Library" | "Backend" ⇒ Right(config)
        case _                                     ⇒ Left(InvalidType)
      }
    }
  }

  private def createIgnoreFile(path: Path) = {
    val ignFile = Paths.get(path.toString, ".gitignore")
    val fileWriter = new FileWriter(ignFile.toFile)

    // This files and dirs should not generally be published
    fileWriter.write("""*.iml
      |*.class
      |*.log
      |dist/
      |boot/
      |logs/
      |lib/
      |out/
      |build/
      |tmp/
      |.history/
      |.idea/
      |.idea_modules/
      |.DS_STORE
      |.cache
      |.settings
      |.project
      |.classpath
    """.stripMargin)
    fileWriter.flush()
  }

  def create(path: Path)(implicit config: Config) = {
    // Initialize Git
    val git = if (config.useGit) {
      Some(Git.init()
        .setDirectory(path.toFile)
        .call())
    } else {
      None
    }

    // Set up the main directories
    val pathString = path.toString
    //path.toFile.mkdirs
    Paths.get(pathString, "src", "main", "matter", config.packageName).toFile.mkdirs()
    Paths.get(pathString, "src", "test").toFile.mkdirs()
    Paths.get(pathString, "build").toFile.mkdirs()

    createIgnoreFile(path)
    val project = createConfig(path, config)

    new Project(project, git)
  }

  def open(path: Path)(implicit config: Config): Either[Error, Project] = {
    val git = if (config.useGit) Some(Git.open(path.toFile)) else None

    var files = List.empty[File]
    val srcDir = Paths.get(path.toString, "src", "main", "matter", config.packageName)
    Files.walkFileTree(srcDir, new SimpleFileVisitor[Path]() {
      override def visitFile(file: Path, attributes: BasicFileAttributes) = {
        files = files :+ file.toFile
        FileVisitResult.CONTINUE
      }
    })

    readConfig(path) map { config ⇒ new Project(config, git, files) }
  }
}
