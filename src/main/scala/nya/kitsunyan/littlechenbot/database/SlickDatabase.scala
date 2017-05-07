package nya.kitsunyan.littlechenbot.database

import info.mukel.telegrambot4s.api.AkkaDefaults

import java.util.Properties

import scala.concurrent.Future
import scala.language.implicitConversions

object SlickDatabase extends AkkaDefaults {
  import slick.jdbc.SQLiteProfile.api._

  private val properties = {
    val properties = new Properties()
    properties.setProperty("foreign_keys", "on")
    properties
  }

  private val database = Database.forURL("jdbc:sqlite:littlechenbot.sqlite",
    prop = properties, driver = "org.sqlite.JDBC")

  private val iqdbConfiguration = TableQuery[IqdbConfigurationTable]
  private val iqdbConfigurationPriority = TableQuery[IqdbConfigurationPriorityTable]
  private val localeConfiguration = TableQuery[LocaleConfigurationTable]

  // Create all tables
  private val createFuture = List(iqdbConfiguration, iqdbConfigurationPriority, localeConfiguration)
    .foldLeft[Future[Unit]](Future.unit) { (future, table) =>
    future.flatMap(_ => database.run(slick.jdbc.meta.MTable.getTables))
      .map(_.map(_.name.name).contains(table.baseTableRow.tableName)).flatMap { exists =>
      if (!exists) {
        database.run(table.schema.create)
      } else {
        Future.unit
      }
    }
  }

  trait Internal extends AkkaDefaults {
    type ReadResult[T] = DBIOAction[T, NoStream, Effect.Read]
    type WriteResult[T] = DBIOAction[T, NoStream, Effect.Write]

    def iqdbConfiguration: TableQuery[IqdbConfigurationTable] = {
      SlickDatabase.iqdbConfiguration
    }

    def iqdbConfigurationPriority: TableQuery[IqdbConfigurationPriorityTable] = {
      SlickDatabase.iqdbConfigurationPriority
    }

    def localeConfiguration: TableQuery[LocaleConfigurationTable] = {
      SlickDatabase.localeConfiguration
    }

    class ExtendedDBIOAction[R](a: DBIOAction[R, NoStream, Nothing]) {
      def run(): Future[R] = {
        // Run only after all tables creation
        createFuture.flatMap(_ => database.run(a))
      }
    }

    implicit def extendedDBIOAction[R](a: DBIOAction[R, NoStream, Nothing]): ExtendedDBIOAction[R] = {
      new ExtendedDBIOAction(a)
    }

    class SwappableOptionDBIOAction[A, B <: Effect](a: Option[DBIOAction[A, NoStream, B]]) {
      def swapOption: DBIOAction[Option[A], NoStream, B] = {
        a.map(_.map(Some.apply)).getOrElse(DBIO.successful(None))
      }
    }

    implicit def swappableOptionDBIOAction[A, B <: Effect](a: Option[DBIOAction[A, NoStream, B]]):
      SwappableOptionDBIOAction[A, B] = {
      new SwappableOptionDBIOAction(a)
    }
  }

  // noinspection TypeAnnotation
  protected class IqdbConfigurationTable(tag: Tag)
    extends Table[IqdbConfigurationData.Configuration](tag, "iqdb_configuration") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def userId = column[Long]("user_id", O.Unique)
    def minSimilarity = column[Option[Int]]("min_similarity")
    def prioritySet = column[Boolean]("priority_set")

    override def * = (id, userId, minSimilarity, prioritySet) <>
      (IqdbConfigurationData.Configuration.tupled, IqdbConfigurationData.Configuration.unapply)
  }

  // noinspection TypeAnnotation
  protected class IqdbConfigurationPriorityTable(tag: Tag)
    extends Table[IqdbConfigurationData.Priority](tag, "iqdb_configuration_priority") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def configurationId = column[Long]("configuration_id")
    def booruService = column[String]("booru_service")

    def configuration = foreignKey("configuration_fk", configurationId, iqdbConfiguration)(_.id,
      ForeignKeyAction.Restrict, ForeignKeyAction.Cascade)

    override def * = (id, configurationId, booruService) <>
      (IqdbConfigurationData.Priority.tupled, IqdbConfigurationData.Priority.unapply)
  }

  // noinspection TypeAnnotation
  protected class LocaleConfigurationTable(tag: Tag)
    extends Table[LocaleConfigurationData.Item](tag, "locale_configuration") {
    def id = column[Long]("id", O.PrimaryKey, O.AutoInc)
    def chatId = column[Long]("chat_id", O.Unique)
    def locale = column[String]("locale")

    override def * = (id, chatId, locale) <>
      (LocaleConfigurationData.Item.tupled, LocaleConfigurationData.Item.unapply)
  }
}
