import scalariform.formatter.preferences._
import com.typesafe.sbt.SbtScalariform
import com.typesafe.sbt.SbtScalariform.ScalariformKeys

name := "matter.compiler"

organization := "de.hyronx"

version := "0.0.1"

scalaVersion := "2.12.1"

crossScalaVersions := Seq("2.11.8", "2.12.1")

enablePlugins(JavaAppPackaging)

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/",
  "SnakeYAML repositroy" at "http://oss.sonatype.org/content/groups/public/"
)

libraryDependencies ++= Seq(

  //"com.chuusai" %% "shapeless" % "2.3.2",
  //"org.typelevel" %% "cats" % "0.8.0",

  //"com.typesafe.akka" %% "akka-actor" % "2.4.12",
  //"com.typesafe.akka" %% "akka-slf4j" % "2.4.12",
  //"com.typesafe.akka" %% "akka-stream" % "2.4.12",
  //"com.typesafe.akka" %% "akka-stream-testkit" % "2.4.12",
  //"com.typesafe.akka" %% "akka-testkit" % "2.4.12" % "test",

  //"com.typesafe.akka" %% "akka-http-core" % "2.4.11",
  //"com.typesafe.akka" %% "akka-http-experimental" % "2.4.11",
  //"com.typesafe.akka" %% "akka-http-spray-json-experimental" % "2.4.11",
  //"com.typesafe.akka" %% "akka-http-testkit" % "2.4.11",

  "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",

  "com.lihaoyi" %% "fastparse" % "0.4.2",
  "com.github.scopt" %% "scopt" % "3.5.0",
  "org.yaml" % "snakeyaml" % "1.18"
)

scalacOptions ++= Seq(
    "-target:jvm-1.8",
    "-deprecation",
    "-encoding", "UTF-8",
    "-feature",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-language:experimental.macros",
    "-unchecked",
    //"-Ywarn-unused-import",
    "-Ywarn-nullary-unit",
    "-Xfatal-warnings",
    "-Xlint",
    //"-Yinline-warnings",
    "-Ywarn-dead-code",
    "-Xfuture")

initialCommands := "import de.hyronx.matter.compiler._"

SbtScalariform.scalariformSettings

ScalariformKeys.preferences := ScalariformKeys.preferences.value
  .setPreference(AlignSingleLineCaseStatements, true)
  .setPreference(DoubleIndentClassDeclaration, true)
  .setPreference(RewriteArrowSymbols, true)
