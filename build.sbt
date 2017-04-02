import com.typesafe.config._

name := "littlechenbot"

version := "1.0"

scalaVersion := "2.12.1"

scalacOptions :=
  "-unchecked" ::
  "-deprecation" ::
  "-feature" ::
  Nil

resolvers += Resolver.sonatypeRepo("snapshots")

libraryDependencies ++=
  "info.mukel" %% "telegrambot4s" % "2.1.0-SNAPSHOT" ::
  "org.slf4j" % "slf4j-simple" % "1.7.25" ::
  "org.scalaj" %% "scalaj-http" % "2.3.0" ::
  "org.json4s" %% "json4s-jackson" % "3.5.0" ::
  "org.xerial" % "sqlite-jdbc" % "3.16.1" ::
  "com.typesafe.slick" %% "slick" % "3.2.0" ::
  Nil

lazy val config = {
  val file = new java.io.File("build.conf")
  if (file.exists && file.canRead) Some(ConfigFactory.parseFile(file)) else None
}

def config[T](getter: Config => String => T, key: String): Option[T] = {
  try {
    config.map(getter(_)(key))
  } catch {
    case _: ConfigException => None
  }
}

proguardSettings

javaOptions in (Proguard, com.typesafe.sbt.SbtProguard.ProguardKeys.proguard) := Seq("-Xmx2G")

ProguardKeys.proguardVersion in Proguard := "5.3.2"

ProguardKeys.options in Proguard ++=
  "-dontoptimize" ::
  "-dontobfuscate" ::
  "-dontnote" ::
  "-dontwarn" ::
  "-ignorewarnings" ::
  Nil

val proguardClassesToKeep: List[String] =
  "akka.actor.DefaultSupervisorStrategy" ::
  "akka.actor.LightArrayRevolverScheduler" ::
  "akka.actor.LocalActorRefProvider" ::
  "akka.actor.LocalActorRefProvider$Guardian" ::
  "akka.actor.LocalActorRefProvider$SystemGuardian" ::
  "akka.dispatch.BoundedControlAwareMessageQueueSemantics" ::
  "akka.dispatch.MultipleConsumerSemantics" ::
  "akka.dispatch.UnboundedControlAwareMessageQueueSemantics" ::
  "akka.dispatch.UnboundedMailbox" ::
  "akka.dispatch.UnboundedMessageQueueSemantics" ::
  "akka.event.DefaultLoggingFilter" ::
  "akka.event.EventStreamUnsubscriber" ::
  "akka.event.LoggerMessageQueueSemantics" ::
  "akka.event.LoggerMailboxType" ::
  "akka.event.Logging$LogExt" ::
  "akka.io.**" ::
  "akka.routing.ConsistentHashingPool" ::
  "akka.routing.RoutedActorCell$RouterActorCreator" ::
  "com.fasterxml.jackson.databind.MapperFeature" ::
  "com.fasterxml.jackson.databind.SerializationFeature" ::
  "com.fasterxml.jackson.databind.DeserializationFeature" ::
  "com.typesafe.sslconfig.ssl.DefaultHostnameVerifier" ::
  "info.mukel.telegrambot4s.**" ::
  "org.slf4j.impl.SimpleLogger" ::
  "org.sqlite.**" ::
  "scala.reflect.ScalaSignature" ::
  Nil

ProguardKeys.options in Proguard ++=
  ProguardOptions.keepMain("nya.kitsunyan.littlechenbot.BotApplication") ::
  proguardClassesToKeep.map("-keep class " + _ + " { *; }")

ProguardKeys.merge in Proguard := true

ProguardKeys.mergeStrategies in Proguard += ProguardMerge.discard("META-INF/.*".r)

ProguardKeys.mergeStrategies in Proguard += ProguardMerge.append("reference.conf")

def solibFilter: List[(String, String)] = {
  import scala.collection.JavaConverters._
  for {
    osList <- config(_.getStringList, "solib.os").toList
    archList <- config(_.getStringList, "solib.arch").toList
    os <- osList.asScala.toList
    arch <- archList.asScala.toList
  } yield (os, arch)
}

ProguardKeys.mergeStrategies in Proguard ++= {
  val list = solibFilter.map { case (os, arch) => s"$os/$arch/" }
  if (list.nonEmpty) {
    val regex = list.reduce(_ + "|" + _)
    ProguardMerge.discard(s"org/sqlite/native/(?!$regex).*?/.*?/.*".r) :: Nil
  } else {
    Nil
  }
}
