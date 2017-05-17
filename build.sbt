import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import Resolvers._
import Dependencies._

initialCommands := "import de.hyronx.matter._"

SbtScalariform.scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(RewriteArrowSymbols, true)

lazy val root = (project in file(".")).
  enablePlugins(JavaAppPackaging, BuildInfoPlugin).
  settings(
    name := "matter",
    organization := "de.hyronx",
    version := "0.0.1",
    scalaVersion := "2.12.2",
    crossScalaVersions := Seq("2.11.8", "2.12.2"),
    logLevel := Level.Warn,

    resolvers ++= Seq(
      releases,
      sonatypeSnapshot,
      snakeYamlRepo,
      jgitRepo
    ),

    libraryDependencies ++= Seq(
      scalaTest,
      scalaCheck,
      fastparse,
      scopt,
      snakeYaml,
      jgit
    ),

    scalacOptions ++= CompileOptions.options,

    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "de.hyronx.matter"
  )
  .dependsOn(RootProject(uri("git://github.com/hyronx/cafebabe")))
