import Dependencies._

resolvers := allResolvers

lazy val commonSettings = Seq(
  organization := "org.im",
  name := "scala-vdom",
  version := "0.1.0",
  scalaVersion := "2.11.6")

lazy val commonScalaJvmOptions = Seq("-Xlint", "-deprecation", "-Xfatal-warnings", "-feature")

lazy val root = (project in file(".")).
  enablePlugins(ScalaJSPlugin).
  settings(libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.8.1",
	  "com.lihaoyi" %% "utest" % "0.3.1" % "test")).
  settings(commonSettings: _*).
  enablePlugins(BuildInfoPlugin).
  settings(
    persistLauncher := true,
    scalaJSStage in Global := FastOptStage,
    buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion, buildInfoBuildNumber),
    buildInfoPackage := "org.im.vdom",
    testFrameworks += new TestFramework("utest.runner.Framework")
    /* ,jsDependencies += RuntimeDOM */)
    
