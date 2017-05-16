package de.hyronx.matter.management

sealed trait Language

object Language {
  object Matter extends Language
  object Scala extends Language
  object Java extends Language
}
