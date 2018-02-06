import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import Resolvers._
import Dependencies._

//initialCommands := "import de.hyronx.matter._"


SbtScalariform.scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(RewriteArrowSymbols, true)


lazy val cafebabe = RootProject(uri("https://github.com/hyronx/cafebabe.git"))

EclipseKeys.skipProject in cafebabe := true

lazy val root = (project in file(".")).
  enablePlugins(BuildInfoPlugin, JavaAppPackaging).
  configs(runDebug).
  dependsOn(cafebabe).
  settings(inConfig(runDebug)(Defaults.configTasks): _*).
  settings(
    name := "matter",
    organization := "de.hyronx",
    version := "0.0.2",
    scalaVersion := "2.12.4",
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
      jgit,
      scalaz,
      sext
    ),

    scalacOptions ++= CompileOptions.options,
    //javaOptions in runDebug ++= Seq("-Xdebug", "-Xrunjdwp:transport=dt_socket,server=y,suspend=y,address=5005"),
    //fork in runDebug := true,

    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage := "de.hyronx.matter"
  )

lazy val runDebug = config("debug").extend(Runtime)
