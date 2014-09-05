import sbt._
import Keys._

object Dependencies{
    def scalaSwing(version: String) = version match { // there is no "scala-swing" for 2.11.1 now, it's in lib-all
      case v if v startsWith "2.10" => "org.scala-lang" % "scala-swing" % v
      case v if v startsWith "2.11" => scalaLibAll(v)
    }
    def scalaLibAll(version: String) = "org.scala-lang" % "scala-library-all" % version // 2.11.x
    def reflectApi(version: String) = "org.scala-lang" % "scala-reflect" % version

    object feh{
      lazy val util = "feh.util" %% "util" % "1.0.4"
    }
  }
