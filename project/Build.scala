import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "to-ban"
    val appVersion      = "1.6.0"

    val appDependencies = Seq(
      // Add your project dependencies here,
      "org.scalaz" %% "scalaz-core" % "6.0.4"
      ,"org.scala-tools.time" %% "time" % "0.5"
      ,"org.mockito" % "mockito-all" % "1.9.0" % "test"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      // Add your own project settings here
    )

}
