package nya.kitsunyan.littlechenbot.util

import com.typesafe.config._

import scala.collection.JavaConverters._

class Configuration private[Configuration] (config: Config) {
  private def config[T](getter: Config => String => T, key: String): Option[T] = {
    try {
      Some(getter(config)(key))
    } catch {
      case _: ConfigException => None
    }
  }

  def int(key: String): Option[Int] = config(_.getInt, key)
  def long(key: String): Option[Long] = config(_.getLong, key)
  def boolean(key: String): Option[Boolean] = config(_.getBoolean, key)
  def string(key: String): Option[String] = config(_.getString, key)

  def stringList(key: String): Option[List[String]] = config(_.getStringList, key).map(_.asScala.toList)

  def configurationList(key: String): List[Configuration] = {
    try {
      config.getObjectList(key).asScala.toList.map(o => new Configuration(o.toConfig))
    } catch {
      case _: ConfigException => Nil
    }
  }

  def configuration(key: String): Option[Configuration] = {
    try {
      Some(new Configuration(config.getObject(key).toConfig))
    } catch {
      case _: ConfigException => None
    }
  }

  def keys: Set[String] = {
    config.entrySet.asScala.map(_.getKey).map(_.split(".").head).toSet
  }
}

object Configuration {
  def apply(fileName: String): Configuration = {
    new Configuration(ConfigFactory.parseFile(new java.io.File(fileName)))
  }
}
