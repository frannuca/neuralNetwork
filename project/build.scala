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

  val logger =  "org.slf4j" % "slf4j-log4j12" % "1.2"

  val matrixdep = "org.fjn" % "matrix_2.9.2" % "1.0.0"


  val specs =  "org.specs2" %% "specs2" % "1.12.2" % "test"
}

object ProjectBuild extends Build {
  import Resolvers._
  import Dependencies._
  import BuildSettings._

  /**
   * top layer  pythia
   */
  lazy val neuralProject = Project (
    "neuralNetwork",
    file ("."),
    settings = buildSettings++ Seq (resolvers :=  Seq(), libraryDependencies ++=Seq(logger,matrixdep,specs))

  ) //aggregate (optimizer,ia, fjn.fjn.fjn.pythia.pricers)



}
