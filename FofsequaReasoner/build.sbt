lazy val root = project

name := "fofsequa-eprover-reasoner"

ThisBuild / organization := "org.nanquanu"
ThisBuild / version := "0.4-SNAPSHOT"

scalaVersion := "2.13.3"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "org.nanquanu" %% "fofsequa" % "0.4-SNAPSHOT"
libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.2.0" % "test"
