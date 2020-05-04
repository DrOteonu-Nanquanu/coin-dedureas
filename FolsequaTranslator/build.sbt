name := "folsequa-tptp-parser"

version := "0.1"

scalaVersion := "2.12.7"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1"
libraryDependencies += "org.nanquanu" % "org.nanquanu.fofsequa" % "0.1" from "file://../Folsequa/target/scala-2.12/folsequa_2.12-0.1.jar"
