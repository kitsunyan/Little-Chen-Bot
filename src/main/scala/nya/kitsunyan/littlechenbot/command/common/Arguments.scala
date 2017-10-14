package nya.kitsunyan.littlechenbot.command.common

import scala.annotation.tailrec

class Arguments(data: String) {
  import Arguments._

  @tailrec private def splitSpaces(index: Int, quote: Boolean, escape: Boolean,
    chars: List[Char], arguments: List[String], lastAmpersand: Boolean, lastVerticalBar: Boolean):
    (List[String], NextMode, Option[String]) = {
    def appendArguments: List[String] = if (chars.nonEmpty) chars.reverse.mkString :: arguments else arguments

    if (index < data.length) {
      val c = data(index)
      if (c <= ' ' && !quote && !escape) {
        splitSpaces(index + 1, quote, escape, Nil, appendArguments, false, false)
      } else {
        if (escape) {
          if (c == 'n') {
            splitSpaces(index + 1, quote, false, '\n' :: chars, arguments, false, false)
          } else {
            splitSpaces(index + 1, quote, false, chars, arguments, false, false)
          }
        } else {
          if (c == '\\') {
            splitSpaces(index + 1, quote, true, chars, arguments, false, false)
          } else if (c == '"') {
            splitSpaces(index + 1, !quote, escape, chars, arguments, false, false)
          } else if (c == '&' && !quote) {
            if (lastAmpersand) {
              (appendArguments.reverse, NextMode.OnSuccess, Some(data.substring(index + 1).trim).filter(_.nonEmpty))
            } else {
              splitSpaces(index + 1, quote, escape, chars, arguments, true, false)
            }
          } else if (c == '|' && !quote) {
            if (lastVerticalBar) {
              (appendArguments.reverse, NextMode.OnFail, Some(data.substring(index + 1).trim).filter(_.nonEmpty))
            } else {
              splitSpaces(index + 1, quote, escape, chars, arguments, false, true)
            }
          } else {
            splitSpaces(index + 1, quote, escape, c :: chars, arguments, false, false)
          }
        }
      }
    } else {
      (appendArguments.reverse, NextMode.None, None)
    }
  }

  @tailrec private def mapArguments(arguments: List[String], targetParameter: Option[String],
    result: Map[String, String]): Map[String, String] = {
    if (arguments.nonEmpty) {
      val argument = arguments.head

      val (parameter, modifiedResult) = if (argument.startsWith("--") && argument.length > 3) {
        (Some(argument.substring(2)), result)
      } else if (argument.startsWith("—") && argument.length > 2) {
        (Some(argument.substring(1)), result)
      } else if (argument.startsWith("-") && argument.length > 1) {
        val parameter = argument.substring(1)

        val (modifiedParameter, modifiedResult) = if (parameter.length > 1) {
          (parameter.charAt(parameter.length - 1).toString, parameter.substring(0, parameter.length - 1)
            .foldLeft(result)((r, c) => r + (c.toString -> "")))
        } else {
          (parameter, result)
        }

        (Some(modifiedParameter), modifiedResult)
      } else {
        (None, result)
      }

      if (parameter.nonEmpty) {
        mapArguments(arguments.tail, parameter, targetParameter.map(p => modifiedResult + (p -> ""))
          .getOrElse(modifiedResult))
      } else {
        mapArguments(arguments.tail, None, modifiedResult + (targetParameter.getOrElse("") -> argument))
      }
    } else {
      targetParameter.map(p => result + (p -> "")).getOrElse(result)
    }
  }

  private val (arguments, nextCommandOption) = {
    val (list, nextMode, nextCommand) = splitSpaces(0, false, false, Nil, Nil, false, false)
    (mapArguments(list, None, Map()), nextCommand.map((nextMode, _)))
  }

  def keySet: Set[String] = arguments.keys.toSet

  class ArgumentValue private[Arguments] (value: Option[String]) {
    def asString: Option[String] = value

    def asStringList: Option[List[String]] = asStringList(",|\\s+")

    def asStringList(splitRegex: String): Option[List[String]] = {
      value.map(_.split(splitRegex).toList).filterNot(_.isEmpty)
    }

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

  def nextCommand: Option[(NextMode, String)] = this.nextCommandOption
}

object Arguments {
  sealed trait NextMode

  object NextMode {
    case object None extends NextMode
    case object OnSuccess extends NextMode
    case object OnFail extends NextMode
  }

  val empty: Arguments = Arguments("")

  def apply(data: String): Arguments = {
    new Arguments(data)
  }
}
