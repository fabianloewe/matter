import sbt._

object Resolvers {
  lazy val releases = Resolver.sonatypeRepo("releases")
  lazy val sonatypeSnapshot = "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
  lazy val snakeYamlRepo = "SnakeYAML repository" at "http://oss.sonatype.org/content/groups/public/"
  lazy val jgitRepo = "jgit-repository" at "https://repo.eclipse.org/content/groups/releases/"
}
