import sbt._
import Keys._


//import com.github.retronym.SbtOneJar

object BuildSettings {


  val buildOrganization = "org.fjn"
  val buildVersion      = "1.0.0"
  val buildScalaVersion = "2.9.2"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  )
}

object Resolvers {


}

object Dependencies {


}

object ProjectBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  /**
   * top layer  pythia
   */
  lazy val pythia = Project (
    "matrix",
    file ("."),
    settings = buildSettings++ Seq (resolvers :=  Seq(), libraryDependencies ++=Seq())

  ) //aggregate (optimizer,ia, fjn.fjn.fjn.pythia.pricers)



}
