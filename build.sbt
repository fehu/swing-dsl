import Build.Dependencies._

name := "swing-dsl"

version := "1.1"

scalaVersion := Build.ScalaVersion

libraryDependencies ++= Seq(scalaSwing, feh.util)