import Dependencies._

name := "scala-vdom root project"

resolvers := allResolvers

lazy val commonSettings = Seq(
  organization := "org.im.vdom",
  version := "0.1.0",
  scalaVersion := "2.11.7")

lazy val commonScalaJvmOptions = Seq("-Xlint", "-deprecation", "-Xfatal-warnings", "-feature")

lazy val root = (project in file(".")).
  aggregate(vdomJS, vdomJVM, component, reactive).
  settings(name := "scala-vdom").
  settings(
    EclipseKeys.useProjectId := true,
    publish := {},
    publishLocal := {})

// crossProject is a Project builder, not a project unto itself
lazy val vdom = crossProject.in(file(".")).
  settings(commonSettings: _*).
  settings(EclipseKeys.useProjectId := true).

  jvmSettings(libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "latest.release" % "test",
	 "org.scalacheck" %% "scalacheck" % "1.12.4" % "test")).

  jsSettings(
    libraryDependencies ++= Seq("org.scala-js" %%% "scalajs-dom" % "latest.release"),
    persistLauncher := true,
    scalaJSStage in Global := FastOptStage
  )
    
lazy val vdomJVM = vdom.jvm
lazy val vdomJS = vdom.js

lazy val component = (project in file("component")).
  dependsOn(vdomJS).
  settings(commonSettings: _*).
  enablePlugins(ScalaJSPlugin).
  settings(
    persistLauncher := true,
    scalaJSStage in Global := FastOptStage)

lazy val reactive = (project in file("reactive")).
  dependsOn(vdomJS).
  settings(commonSettings: _*).
  settings(libraryDependencies += "org.monifu" %%% "monifu" % "1.0-M1").
  enablePlugins(ScalaJSPlugin).
  settings(
    persistLauncher := true,
    scalaJSStage in Global := FastOptStage)

