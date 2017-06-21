object CompileOptions {
  lazy val options = Seq(
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
      //"-Xfatal-warnings",
      "-Xlint",
      //"-Yinline-warnings",
      "-Ywarn-dead-code",
      "-Xfuture")
}
