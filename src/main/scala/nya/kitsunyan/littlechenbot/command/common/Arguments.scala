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
          if (c == 'n') {
            splitSpaces(index + 1, quote, false, '\n' :: chars, arguments)
          } else {
            splitSpaces(index + 1, quote, false, chars, arguments)
          }
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

  @tailrec private def mapArguments(arguments: List[String], targetParameter: Option[String],
    result: Map[String, String]): Map[String, String] = {
    if (arguments.nonEmpty) {
      val argument = arguments.head

      val parameter = if (argument.startsWith("--") && argument.length > 3) {
        Some(argument.substring(2))
      } else if (argument.startsWith("—") && argument.length > 2) {
        Some(argument.substring(1))
      } else if (argument.startsWith("-") && argument.length == 2) {
        Some(argument.substring(1))
      } else {
        None
      }

      if (parameter.nonEmpty) {
        mapArguments(arguments.tail, parameter, targetParameter.map(p => result + (p -> "")).getOrElse(result))
      } else {
        mapArguments(arguments.tail, None, result + (targetParameter.getOrElse("") -> argument))
      }
    } else {
      targetParameter.map(p => result + (p -> "")).getOrElse(result)
    }
  }

  private val arguments = mapArguments(splitSpaces(0, false, false, Nil, Nil), None, Map())

  def keySet: Set[String] = arguments.keys.toSet

  class ArgumentValue private[Arguments] (value: Option[String]) {
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

    def asLong: Option[Long] = {
      value.flatMap { value =>
        try {
          Some(value.toLong)
        } catch {
          case _: NumberFormatException => None
        }
      }
    }

    def nonEmpty: Boolean = value.nonEmpty
  }

  def apply[T](keys: String*): ArgumentValue = {
    new ArgumentValue(arguments.filterKeys(keys.contains).values.headOption)
  }

  def freeValue: ArgumentValue = {
    new ArgumentValue(arguments.get(""))
  }
}

object Arguments {
  val empty: Arguments = Arguments("")

  def apply(data: String): Arguments = {
    new Arguments(data)
  }
}
