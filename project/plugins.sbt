logLevel := Level.Warn

addSbtPlugin("com.lightbend.sbt" % "sbt-proguard" % "0.3.0")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.6")

libraryDependencies += "com.typesafe" % "config" % "1.3.3"
