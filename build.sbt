import Build.Dependencies._

name := "swing" // -dsl

version := "1.1"

libraryDependencies <+= scalaVersion(scalaSwing)

libraryDependencies ++= Seq(feh.util)