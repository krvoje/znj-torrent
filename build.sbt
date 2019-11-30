name := "znj-torrent"

version := "1.0"

scalaVersion := "2.13.1"

// Read here for optional jars and dependencies
libraryDependencies ++= Seq(


  "org.typelevel" %% "cats-core" % "2.0.0",
  "joda-time" % "joda-time" % "2.9.9",
  "com.chuusai" %% "shapeless" % "2.3.3",

  "com.typesafe.akka" %% "akka-actor-typed" % "2.5.26",
  "com.typesafe.akka" %% "akka-http" % "10.1.10",
  "com.typesafe.akka" %% "akka-stream" % "2.5.25",

  "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.25" % Test,
  "org.specs2" %% "specs2-core" % "4.6.0" % Test,
  //"com.typesafe.akka" %% "akka-testkit" % "2.5.4"
  //"com.typesafe.akka" %% "akka-http-testkit" % "10.0.9"
  //"com.typesafe.akka" %% "akka-actor-testkit" % "2.5.4"
)

resolvers ++= Seq(
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("snapshots")
)

//scalacOptions += "-Ypartial-unification"
scalacOptions in Test ++= Seq("-Yrangepos")
