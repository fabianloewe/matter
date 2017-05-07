package de.hyronx.matter.management

import java.nio.file.{ Path, Paths }
import java.io.FileWriter

import org.yaml.snakeyaml.Yaml
import org.yaml.snakeyaml.representer.Representer
import org.yaml.snakeyaml.nodes.{ Tag, NodeTuple }
import org.yaml.snakeyaml.introspector.Property

import org.eclipse.jgit.api.Git

import de.hyronx.matter.Config

class Project private (
  val git: Git
)

object Project {
  class ProjectRepresenter extends Representer {
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
    fileWriter.flush
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
    fileWriter.flush
  }

  def create(path: Path)(implicit config: Config) = {
    // Initialize Git
    val git = Git.init
      .setDirectory(path.toFile)
      .call

    // Set up the main directories
    val pathString = path.toString
    //path.toFile.mkdirs
    Paths.get(pathString, "src", "main", "matter", config.packageName).toFile.mkdirs
    Paths.get(pathString, "src", "test").toFile.mkdirs
    Paths.get(pathString, "build").toFile.mkdirs

    createIgnoreFile(path)
    createConfig(path, config)

    new Project(git)
  }
}
