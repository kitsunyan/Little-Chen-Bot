package nya.kitsunyan.littlechenbot.database

import nya.kitsunyan.littlechenbot.util.Locale

import scala.concurrent.Future

object LocaleConfigurationData extends SlickDatabase.Internal {
  import slick.jdbc.SQLiteProfile.api._

  case class Item(id: Long, chatId: Long, locale: String)

  def get(chatId: Long): Future[Option[Locale]] = {
    localeConfiguration.filter(_.chatId === chatId).result.headOption.run()
      .map(_.flatMap(i => Locale.get(i.locale)))
  }

  def set(chatId: Long, locale: Locale): Future[Unit] = {
    localeConfiguration.filter(_.chatId === chatId).delete
      .flatMap(_ => localeConfiguration.returning(localeConfiguration.map(_.id))
        .into((item, id) => item.copy(id = id)) += Item(0, chatId, locale.name))
      .transactionally.run()
      .map(_ => ())
  }
}
