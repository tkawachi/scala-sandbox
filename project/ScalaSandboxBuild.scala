import sbt._
import sbt.Keys._

object ScalaSandboxBuild extends Build {

  lazy val scalaSandbox = Project(
    id = "scala-sandbox",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Scala Sandbox",
      organization := "net.pikot",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.1"
      // add other settings here
    )
  )
}
