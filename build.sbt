name := "littlechenbot"

version := "1.0"

scalaVersion := "2.12.1"

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies += "info.mukel" %% "telegrambot4s" % "2.1.0-SNAPSHOT"

libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.3.0"

libraryDependencies += "org.json4s" %% "json4s-jackson" % "3.5.0"

libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.16.1"

libraryDependencies += "com.typesafe.slick" %% "slick" % "3.2.0"

mainClass in assembly := Some("nya.kitsunyan.littlechenbot.BotApplication")

assemblyJarName in assembly := "littlechenbot.jar"
