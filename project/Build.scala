import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "sapsan"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    jdbc,
    anorm,
    //"com.typesafe.slick" %% "slick" % "1.0.1",
    //"org.squeryl" %% "squeryl" % "0.9.5-6",
    "mysql" % "mysql-connector-java" % "5.1.21"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
