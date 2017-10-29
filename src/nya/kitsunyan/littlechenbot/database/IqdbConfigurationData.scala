package nya.kitsunyan.littlechenbot.database

import scala.concurrent.Future

object IqdbConfigurationData extends SlickDatabase.Internal {
  import slick.jdbc.SQLiteProfile.api._

  case class Item(userId: Long, minSimilarity: Option[Int], priority: Option[List[String]])
  case class Configuration(id: Long, userId: Long, minSimilarity: Option[Int], prioritySet: Boolean)
  case class Priority(id: Long, configurationId: Long, booruService: String)

  def get(userId: Long): Future[Option[Item]] = {
    def queryConfiguration: ReadResult[Option[Configuration]] = {
      iqdbConfiguration.filter(_.userId === userId).result.headOption
    }

    def queryPriority(c: Configuration): ReadResult[Iterable[Priority]] = {
      iqdbConfigurationPriority.filter(_.configurationId === c.id).result
    }

    def mapItem(c: Configuration)(p: Iterable[Priority]): Item = {
      Item(c.userId, c.minSimilarity, if (c.prioritySet) Some(p.map(_.booruService).toList) else None)
    }

    queryConfiguration
      .flatMap(_.map(c => queryPriority(c).map(mapItem(c))).swapOption)
      .transactionally.run()
  }

  def set(item: Item): Future[Unit] = {
    def insertConfiguration(): WriteResult[Configuration] = {
      iqdbConfiguration
        .returning(iqdbConfiguration.map(_.id))
        .into((item, id) => item.copy(id = id)) +=
          Configuration(0, item.userId, item.minSimilarity, item.priority.nonEmpty)
    }

    def insertPriority(c: Configuration): WriteResult[Option[Iterable[Priority]]] = {
      item.priority.map { priority =>
        iqdbConfigurationPriority
          .returning(iqdbConfigurationPriority.map(_.id))
          .into((item, id) => item.copy(id = id)) ++= priority.map(Priority(0, c.id, _))
      }.swapOption
    }

    iqdbConfiguration.filter(_.userId === item.userId).delete
      .flatMap(_ => insertConfiguration())
      .flatMap(insertPriority)
      .transactionally.run()
      .map(_ => ())
  }

  def delete(userId: Long): Future[Unit] = {
    iqdbConfiguration.filter(_.userId === userId).delete.run().map(_ => ())
  }
}
