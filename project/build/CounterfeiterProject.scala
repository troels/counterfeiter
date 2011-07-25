import sbt._

final class CounterfeiterProject(info: ProjectInfo) extends DefaultProject(info) { 
  val guava = "com.google.guava" % "guava" % "r09"
  val scalatestDependency = "org.scalatest" % "scalatest_2.9.0" % "1.6.1" % "test"
}

