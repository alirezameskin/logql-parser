import Dependencies._

name := "logql-parser"
organization := "com.github.alirezameskin"

version := "0.0.1-SNAPSHOT"
scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  parserCombinators,
  scalaTest % Test
)

githubOwner := "alirezameskin"
githubRepository := "logql-parser"
githubTokenSource := TokenSource.Or(TokenSource.Environment("GITHUB_TOKEN"), TokenSource.GitConfig("github.token"))
