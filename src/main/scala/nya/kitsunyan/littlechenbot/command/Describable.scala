package nya.kitsunyan.littlechenbot.command

trait Describable {
  case class Description(commands: List[String], text: String)

  def prependDescription(list: List[Description]): List[Description] = list
}
