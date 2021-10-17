name := "twutter"

version := "0.1"

scalaVersion := "3.1.0-RC1"

libraryDependencies += "com.github.ghostdogpr" %% "caliban" % "1.2.0"
libraryDependencies += "com.github.ghostdogpr" %% "caliban-zio-http"   % "1.2.0"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
libraryDependencies += "dev.zio" %% "zio-streams" % "1.0.12"
libraryDependencies += "com.faunadb" % "faunadb-java" % "4.2.0"