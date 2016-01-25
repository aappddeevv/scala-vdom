import sbt._

object Dependencies {

  val allResolvers = Seq(
    Resolver.url("file://" + Path.userHome.absolutePath + "/.ivy/local"),
    "sonatype" at "https://oss.sonatype.org/content/repositories/releases")

  //val frontendDeps = Seq("org.scala-js" %%% "scalajs-dom" % "0.8.1")

  val scalatest = "org.scalatest" %% "scalatest" % "3.0.0-M15"

  //val scalaJsDependencies = Seq("org.scala-js" %%% "scalajs-dom" % "0.8.1")
  
  val testDeps = Seq(scalatest % Test)

}
