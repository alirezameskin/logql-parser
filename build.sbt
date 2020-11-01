import Dependencies._

name := "logql-parser"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  parserCombinators,
  scalaTest % Test
)
