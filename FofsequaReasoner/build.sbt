name := "test"

ThisBuild / organization := "org.nanquanu"
ThisBuild / version := "0.2-SNAPSHOT"

scalaVersion := "2.12.7"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.nanquanu" %% "fofsequa" % "0.2-SNAPSHOT"
libraryDependencies += "org.scalatest" % "scalatest_2.12" % "3.2.0" % "test"