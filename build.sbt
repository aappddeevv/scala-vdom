import Dependencies._

name := "scala-vdom root project"

resolvers := allResolvers

lazy val commonSettings = Seq(
  organization := "org.im.vdom",
  version := "0.1.0",
  scalaVersion := "2.11.7"
)

lazy val commonScalacOptions = Seq("-Xlint", "-deprecation", "-Xfatal-warnings", "-feature")


lazy val root = (project in file(".")).
  aggregate(vdomJS, vdomJVM, component, reactive).
  settings(name := "scala-vdom").
  settings(
    publish := {},
    publishLocal := {})

// crossProject is a Project builder, not a project unto itself
lazy val vdom = crossProject.in(file(".")).
	settings(scalacOptions ++= commonScalacOptions).
  settings(commonSettings: _*).
  settings(libraryDependencies ++= Seq("org.scalatest" %%% "scalatest" % "3.0.0-M15" % "test")).

  jvmSettings(libraryDependencies ++= Seq("org.scalacheck" %% "scalacheck" % "1.12.5" % "test")).

  jsSettings(
    relativeSourceMaps := true,
	jsDependencies += RuntimeDOM % "test",	
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
  settings(libraryDependencies += "org.monifu" %%% "monifu" % "latest.release").
  enablePlugins(ScalaJSPlugin).
  settings(
    persistLauncher := true,
    scalaJSStage in Global := FastOptStage)

