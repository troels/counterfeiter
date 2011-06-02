import sbt._

final class CounterfeiterProject(info: ProjectInfo) extends DefaultProject(info) { 
  val scalaToolsSnapshots = "Scala Tools Snapshots" at "http://scala-tools.org/repo-snapshots/"

  val scalazDependency = "org.scalaz" %% "scalaz-core" % "6.0-SNAPSHOT"
  val scalatestDependency = "org.scalatest" % "scalatest" % "1.2" % "test"
  val junit = "junit" % "junit" % "4.8.1"
  val guava = "com.google.guava" % "guava" % "r09"
}

