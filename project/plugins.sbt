logLevel := Level.Warn

resolvers += Resolver.sonatypeRepo("releases")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.1.5")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "5.1.0")