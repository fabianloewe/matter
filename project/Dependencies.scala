import sbt._

object Dependencies {
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

    lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.0" % "test"
    lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"
    lazy val fastparse = "com.lihaoyi" %% "fastparse" % "0.4.2"
    lazy val scopt = "com.github.scopt" %% "scopt" % "3.5.0"
    lazy val snakeYaml = "org.yaml" % "snakeyaml" % "1.18"
    lazy val jgit = "org.eclipse.jgit" % "org.eclipse.jgit" % "4.7.0.201704051617-r"

}
