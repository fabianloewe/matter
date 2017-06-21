package de.hyronx.matter.management

import scala.beans.BeanProperty

import de.hyronx.matter.BuildInfo

case class MatterInfo(
    @BeanProperty var version: String = BuildInfo.version
) {
  // Needed by SnakeYAML for reading YAML...
  def this() = this(BuildInfo.version)
}

case class ProjectInfo(
    @BeanProperty var name: String,
    @BeanProperty var vendor: String,
    @BeanProperty var version: String,
    @BeanProperty var `type`: String = "Application"
) {
  // Needed by SnakeYAML for reading YAML...
  def this() = this(null, null, null, null)
}

case class ProjectConfig(
    @BeanProperty var matter: MatterInfo = MatterInfo(),
    @BeanProperty var project: ProjectInfo
) {
  // Needed by SnakeYAML for reading YAML...
  def this() = this(new MatterInfo(), new ProjectInfo())
}
