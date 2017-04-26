package nya.kitsunyan.littlechenbot.util

trait UserMessageException {
  this: Throwable =>

  val userMessage: Option[String]
}
