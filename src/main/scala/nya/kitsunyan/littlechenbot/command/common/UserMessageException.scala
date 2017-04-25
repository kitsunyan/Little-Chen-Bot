package nya.kitsunyan.littlechenbot.command.common

trait UserMessageException {
  this: Throwable =>

  val userMessage: Option[String]
}
