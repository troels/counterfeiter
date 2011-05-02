import sbt._

final class CounterfeiterProject(info: ProjectInfo) extends DefaultProject(info) { 
  
  val scalaDependency = "org.scalaz" %% "scalaz-core"
  val scalatestDependency = "org.scalatest" % "scalatest" % "1.2" % "test"
  val junit = "junit" % "junit" % "4.8.1"
}

