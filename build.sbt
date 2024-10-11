name := "znj-torrent"

version := "1.0"

scalaVersion := "3.3.3"

val PekkoVersion = "1.1.2"

// Read here for optional jars and dependencies
libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.12.7",

  "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion,
  "org.apache.pekko" %% "pekko-http" % "1.0.1",
  "org.apache.pekko" %% "pekko-stream" % PekkoVersion,

  "org.apache.pekko" %% "pekko-stream-testkit" % PekkoVersion % Test,
  "org.apache.pekko" %% "pekko-testkit" % PekkoVersion % Test,
  "org.apache.pekko" %% "pekko-http-testkit" % "1.0.1" % Test,
  "org.apache.pekko" %% "pekko-actor-testkit-typed" % PekkoVersion % Test,

  "org.scalactic" %% "scalactic" % "3.2.19" % Test,
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

resolvers ++= Resolver.sonatypeOssRepos("releases")
resolvers ++= Resolver.sonatypeOssRepos("snapshots")

Global / onChangedBuildSource := ReloadOnSourceChanges
