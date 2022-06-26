import Dependencies._

ThisBuild / scalaVersion     := "2.13.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "sjdfsa",
    //libraryDependencies += scalaTest % Test
    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.2.12",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.12" % "test"
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.

enablePlugins(ScalaJSPlugin)

name := "sjdfsa"
scalaVersion := "2.13.1" // or any other Scala version >= 2.11.12

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.1.0"
