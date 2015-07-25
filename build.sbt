import Dependencies._

name := "scala-vdom root project"

resolvers := allResolvers

lazy val commonSettings = Seq(
  organization := "org.im",
  name := "scala-vdom",
  version := "0.1.0",
  scalaVersion := "2.11.7")

lazy val commonScalaJvmOptions = Seq("-Xlint", "-deprecation", "-Xfatal-warnings", "-feature")

lazy val root = (project in file(".")).
  aggregate(vdomJS, vdomJVM).
  settings(
    EclipseKeys.useProjectId := true,
    publish := {},
    publishLocal := {})

lazy val vdom = crossProject.in(file(".")).
  settings(commonSettings: _*).
  settings(EclipseKeys.useProjectId := true).
  jvmSettings(
	libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "latest.release" % "test")
).
  jsSettings(
    libraryDependencies ++= Seq("org.scala-js" %%% "scalajs-dom" % "0.8.1"),
    persistLauncher := true,
    scalaJSStage in Global := FastOptStage
  )
    
lazy val vdomJVM = vdom.jvm
lazy val vdomJS = vdom.js
