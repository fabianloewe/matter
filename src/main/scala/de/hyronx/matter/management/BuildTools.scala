package de.hyronx.matter.management

sealed trait BuildTool

object BuildTool {
  object Sbt extends BuildTool
}
