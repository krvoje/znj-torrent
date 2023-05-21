name := "znj-torrent"

version := "1.0"

scalaVersion := "3.2.1"

// Read here for optional jars and dependencies
libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.12.2",

  "com.typesafe.akka" %% "akka-actor-typed" % "2.8.0",
  "com.typesafe.akka" %% "akka-http" % "10.5.0",
  "com.typesafe.akka" %% "akka-stream" % "2.8.0",

  "com.typesafe.akka" %% "akka-stream-testkit" % "2.8.0" % Test,
  "org.specs2" %% "specs2-core" % "4.19.2" % Test,
  //"com.typesafe.akka" %% "akka-testkit" % "2.5.4"
  //"com.typesafe.akka" %% "akka-http-testkit" % "10.0.9"
  //"com.typesafe.akka" %% "akka-actor-testkit" % "2.5.4"
)

resolvers ++= Resolver.sonatypeOssRepos("releases")
resolvers ++= Resolver.sonatypeOssRepos("snapshots")

Global / onChangedBuildSource := ReloadOnSourceChanges
