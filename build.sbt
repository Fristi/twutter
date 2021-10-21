name := "twutter"

version := "0.1"

scalaVersion := "3.1.0"

libraryDependencies ++= Seq(
  "com.github.ghostdogpr" %% "caliban" % "1.2.0",
  "com.github.ghostdogpr" %% "caliban-zio-http" % "1.2.0",
  "org.typelevel" %% "cats-core" % "2.6.1",
  "dev.zio" %% "zio-streams" % "1.0.12",
  "com.faunadb" % "faunadb-java" % "4.2.0",
  "org.slf4j" % "slf4j-api" % "1.7.32",
  "org.slf4j" % "slf4j-simple" % "1.7.32",
  "org.typelevel" %% "kittens" % "3.0.0-M1"
)

Compile / mainClass := Some("twutter.TwutterServer")