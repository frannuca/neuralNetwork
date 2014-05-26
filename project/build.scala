import sbt._
import Keys._


//import com.github.retronym.SbtOneJar

object BuildSettings {


  val buildOrganization = "org.fjn"
  val buildVersion      = "1.0.0"
  val buildScalaVersion = "2.10.3"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  )
}

object Resolvers {

  val typesafe = "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
  val sonatype1 = "Sonatype OSS Releases" at "http://oss.sonatype.org/content/repositories/releases/"
  val sonatype2 = "Sonatype OSS Snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
}

object Dependencies {


  val actors = "com.typesafe.akka" %% "akka-actor" % "2.2.3"
  val apacheMath = "org.apache.commons" % "commons-math3" % "3.0"
  val rmq = "com.rabbitmq" % "amqp-client" % "3.2.3"
  val proto = "com.google.protobuf" % "protobuf-java" % "2.5.0"
  val akkaConf = "com.typesafe" % "config" % "1.2.0"
  val guava = "com.google.guava" % "guava" % "12.0"
  val junit = "junit" % "junit" % "4.11"
  val scalaTest = "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

  val scalaReflect = "org.scala-lang" % "scala-reflect" % BuildSettings.buildScalaVersion
  val log4j = "log4j" % "log4j" % "1.2.14"
  val matrixdep = "org.fjn" %% "matrix" % "1.0.0"
    val optimization = "org.fjn" %% "optimization" % "1.0.0"

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
    settings = buildSettings++ Seq (resolvers :=  Seq(), 
      libraryDependencies ++=Seq(actors, scalaTest, rmq, proto, apacheMath, junit, scalaReflect, guava,log4j,matrixdep,optimization))

  ) //aggregate (optimizer,ia, fjn.fjn.fjn.pythia.pricers)



}
