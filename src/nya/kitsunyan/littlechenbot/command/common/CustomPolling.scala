package nya.kitsunyan.littlechenbot.command.common

import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.methods._
import info.mukel.telegrambot4s.models._

import akka.NotUsed
import akka.stream.scaladsl.Source

import scala.concurrent.Future
import scala.util.control.NonFatal

trait CustomPolling extends BotBase with AkkaImplicits with BotExecutionContext {
  private val pollingInterval = 50
  private val sleepInterval = 2000

  private val updates: Source[Update, NotUsed] = {
    val seed = Future.successful(0L, Seq.empty[Update])

    val iterator = Iterator.iterate(seed)(_.flatMap { case (offset, newUpdates) =>
      val maxOffset = newUpdates.map(_.updateId).fold(offset)(_ max _)
      request(GetUpdates(Some(maxOffset + 1), timeout = Some(pollingInterval))).recover {
        case NonFatal(e) =>
          logger.error("GetUpdates failed", e)
          Thread.sleep(sleepInterval)
          Seq.empty[Update]
      }.map((maxOffset, _))
    })

    Source.fromIterator(() => iterator)
      .mapAsync(Runtime.getRuntime.availableProcessors)(_.map(_._2))
      .mapConcat(_.to)
  }

  def getMe: Future[User] = {
    request(GetMe).recoverWith { case _ =>
      Thread.sleep(sleepInterval)
      getMe
    }
  }

  override def run(): Unit = {
    updates.runForeach { update =>
      try {
        receiveUpdate(update)
      } catch {
        case NonFatal(e) => logger.error("Caught exception in update handler", e)
      }
    }
  }

  override def shutdown(): Future[Unit] = {
    system.terminate().transformWith { _ =>
      logger.info("Shutting down polling")
      system.terminate() map (_ => ())
    }
  }
}
