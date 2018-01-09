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

  @tailrec private def mapArguments(tokens: List[String], targetParameter: Option[String],
    arguments: Map[String, String], freeArguments: List[String], optionsEnd: Boolean):
    (Map[String, String], List[String]) = {
    if (tokens.nonEmpty) {
      val token = tokens.head

      if (!optionsEnd && (token == "--" || token == "—")) {
        mapArguments(tokens.tail, None, arguments, freeArguments, true)
      } else {
        val (parameter, modifiedArguments) = if (!optionsEnd && token.startsWith("--") && token.length > 3) {
          (Some(token.substring(2)), arguments)
        } else if (!optionsEnd && token.startsWith("—") && token.length > 2) {
          (Some(token.substring(1)), arguments)
        } else if (!optionsEnd && token.startsWith("-") && token.length > 1) {
          val parameter = token.substring(1)

          val (modifiedParameter, modifiedArguments) = if (parameter.length > 1) {
            (parameter.charAt(parameter.length - 1).toString, parameter.substring(0, parameter.length - 1)
              .foldLeft(arguments)((r, c) => r + (c.toString -> "")))
          } else {
            (parameter, arguments)
          }

          (Some(modifiedParameter), modifiedArguments)
        } else {
          (None, arguments)
        }

        if (parameter.nonEmpty) {
          mapArguments(tokens.tail, parameter, targetParameter.map(p => modifiedArguments + (p -> ""))
            .getOrElse(modifiedArguments), freeArguments, optionsEnd)
        } else {
          targetParameter match {
            case Some(targetParameter) =>
              mapArguments(tokens.tail, None, modifiedArguments + (targetParameter -> token), freeArguments, optionsEnd)
            case None =>
              mapArguments(tokens.tail, None, modifiedArguments, token :: freeArguments, optionsEnd)
          }
        }
      }
    } else {
      (targetParameter.map(p => arguments + (p -> "")).getOrElse(arguments), freeArguments.reverse)
    }
  }

  private val (arguments, freeValues, nextCommandOption) = {
    val (tokens, nextMode, nextCommand) = splitSpaces(0, false, false, Nil, Nil, false, false)
    val (arguments, freeValues) = mapArguments(tokens, None, Map(), Nil, false)
    (arguments, freeValues, nextCommand.map((nextMode, _)))
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

    def asIntList: Option[List[Int]] = {
      try {
        asStringList.map(_.flatMap(v => "^(-?\\d+)-(-?\\d+)".r.findFirstMatchIn(v).map(_.subgroups).flatMap {
          case start :: end :: Nil => Some(start.toInt to end.toInt)
          case _ => None
        }.getOrElse(List(v.toInt))).distinct.sorted)
      } catch {
        case _: NumberFormatException => None
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

  lazy val free: List[ArgumentValue] = freeValues.map(value => new ArgumentValue(Some(value)))

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
