name := "untitled"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.12",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "com.typesafe.akka" %% "akka-slf4j" % "2.4.0",
  "com.typesafe.akka" %% "akka-actor" % "2.4.0",
  "com.typesafe.akka" %% "akka-http-experimental" % "2.0-M1",
  "com.typesafe.akka" %% "akka-http-spray-json-experimental" % "2.0-M1",
  "com.typesafe.akka" %% "akka-http-testkit-experimental" % "2.0-M1" % "test",
  "org.scalatest" %% "scalatest" % "2.2.5" % "test"

)