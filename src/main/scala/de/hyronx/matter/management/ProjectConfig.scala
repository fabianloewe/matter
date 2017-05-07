package de.hyronx.matter.management

import scala.beans.BeanProperty

import de.hyronx.matter.BuildInfo

case class MatterInfo(
  @BeanProperty var version: String = BuildInfo.version
)

case class ProjectInfo(
  @BeanProperty var name: String,
  @BeanProperty var vendor: String,
  @BeanProperty var version: String
)

case class ProjectConfig(
  @BeanProperty var matter: MatterInfo = MatterInfo(),
  @BeanProperty var project: ProjectInfo
)
