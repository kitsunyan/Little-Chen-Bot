logLevel := Level.Warn

addSbtPlugin("com.typesafe.sbt" % "sbt-proguard" % "0.2.3")
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.6")

libraryDependencies += "com.typesafe" % "config" % "1.3.1"
