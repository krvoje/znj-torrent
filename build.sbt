name := "znj-torrent"

version := "1.0"

scalaVersion := "2.12.8"

// Read here for optional jars and dependencies
libraryDependencies ++= Seq(
  "joda-time" % "joda-time" % "2.9.9"
  , "com.chuusai" %% "shapeless" % "2.3.3"
  , "com.typesafe.akka" %% "akka-actor" % "2.5.4"
  , "com.typesafe.akka" %% "akka-http" % "10.0.9"
  , "com.typesafe.akka" %% "akka-stream" % "2.5.4"

  , "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.4" % Test
  , "org.specs2" %% "specs2-core" % "3.9.1" % Test
  //, "com.typesafe.akka" %% "akka-testkit" % "2.5.4"
  //, "com.typesafe.akka" %% "akka-http-testkit" % "10.0.9"
  //, "com.typesafe.akka" %% "akka-actor-testkit" % "2.5.4"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

scalacOptions += "-Ypartial-unification"
scalacOptions in Test ++= Seq("-Yrangepos")
