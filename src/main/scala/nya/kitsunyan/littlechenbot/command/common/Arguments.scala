package nya.kitsunyan.littlechenbot.command.common

import scala.annotation.tailrec

class Arguments(data: String) {
  @tailrec private def splitSpaces(index: Int, quote: Boolean, escape: Boolean,
    chars: List[Char], arguments: List[String]): List[String] = {
    if (index < data.length) {
      val c = data(index)
      if (c <= ' ' && !quote && !escape) {
        splitSpaces(index + 1, quote, escape, Nil,
          if (chars.nonEmpty) chars.reverse.mkString :: arguments else arguments)
      } else {
        if (escape) {
          splitSpaces(index + 1, quote, false, c :: chars, arguments)
        } else {
          if (c == '\\') {
            splitSpaces(index + 1, quote, true, chars, arguments)
          } else if (c == '"') {
            splitSpaces(index + 1, !quote, escape, chars, arguments)
          } else {
            splitSpaces(index + 1, quote, escape, c :: chars, arguments)
          }
        }
      }
    } else {
      (if (chars.nonEmpty) chars.reverse.mkString :: arguments else arguments).reverse
    }
  }

  @tailrec private def mapArguments(arguments: List[String], what: Option[String],
    result: Map[String, String]): Map[String, String] = {
    if (arguments.nonEmpty) {
      val argument = arguments.head
      if (argument.startsWith("-") || argument.startsWith("—")) {
        mapArguments(arguments.tail, Some(argument), what.map(w => result + (w -> "")).getOrElse(result))
      } else {
        mapArguments(arguments.tail, None, what.map(w => result + (w -> argument)).getOrElse(result))
      }
    } else {
      what.map(w => result + (w -> "")).getOrElse(result)
    }
  }

  private val arguments = mapArguments(splitSpaces(0, false, false, Nil, Nil), None, Map())

  private def option(prefix: String, key: String): Option[String] = {
    arguments.get(prefix + key)
  }

  class ArgumentValue(value: Option[String]) {
    def asString: Option[String] = value

    def asInt: Option[Int] = {
      value.flatMap { value =>
        try {
          Some(value.toInt)
        } catch {
          case _: NumberFormatException => None
        }
      }
    }

    def nonEmpty: Boolean = value.nonEmpty
  }

  def apply(shortKey: Option[String], longKey: String): ArgumentValue = {
    new ArgumentValue(shortKey.flatMap(option("-", _)) orElse option("--", longKey) orElse option("—", longKey))
  }

  def apply[T](shortKey: String, longKey: String): ArgumentValue = {
    this(Some(shortKey), longKey)
  }

  def apply[T](longKey: String): ArgumentValue = {
    this(None, longKey)
  }
}
