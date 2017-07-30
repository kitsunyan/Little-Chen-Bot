package nya.kitsunyan.littlechenbot.command

import nya.kitsunyan.littlechenbot.command.common._

import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout

import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.concurrent.Future

trait ForemanCommand extends Command {
  private case class CommandKey(chatId: Long, command: String)

  private case class CommandData(count: Int, time: Long)

  private case class CheckCommand(key: CommandKey)

  private val actor = ActorSystem("ForemanCommand").actorOf(Props(new ForemanActor))
  private implicit val timeout = Timeout(1, TimeUnit.DAYS)

  private class ForemanActor extends Actor {
    val commandMap = new mutable.HashMap[CommandKey, CommandData]

    def receive: PartialFunction[Any, Unit] = {
      case CheckCommand(commandKey) =>
        val time = System.currentTimeMillis
        val commandData = commandMap
          .get(commandKey)
          .filter(_.time + 90 * 1000 >= time)
          .map(c => CommandData(c.count + 1, time))
          .getOrElse(CommandData(1, time))

        if (commandData.count >= 3) {
          commandMap.remove(commandKey)
          sender ! true
        } else {
          commandMap += commandKey -> commandData
          sender ! false
        }
    }
  }

  def foreignCommands(chatId: Long): Option[List[String]]

  def foremanImage: Option[String]

  override def handleMessage(message: ExtendedMessage, filterChat: FilterChat): Future[Status] = {
    foremanImage
      .filter(_ => filterChat.soft && !filterChat.filtered && message.firstCommand)
      .flatMap { foremanImage =>
      (message.initial.text orElse message.initial.caption)
        .flatMap("^/(\\w+)$".r.findFirstMatchIn)
        .flatMap(_.subgroups.headOption)
        .filterNot(foreignCommands(message.initial.chat.id).map(l => l.contains _).getOrElse(_ => true))
        .map(c => (actor ? CheckCommand(CommandKey(message.initial.chat.id, c))).mapTo[Boolean])
        .map(_.map { shouldReply =>
        if (shouldReply) {
          request(SendPhoto(message.initial.source, InputFile(foremanImage),
            replyToMessageId = Some(message.initial.messageId)))
            .recover(handleException(Some(message.initial))(_))
        } else {
          Future.unit
        }
      })
    }.getOrElse(Future.unit)
      .flatMap(_ => super.handleMessage(message, filterChat))
  }
}
