package nya.kitsunyan.littlechenbot.util

sealed trait Locale {
  val name: String

  val A_LIST_OF_TAGS_TO_PUZZLE_A_CHARACTER: String
  val ADDITIONAL_RESULTS_FS: String
  val AN_EXCEPTION_WAS_THROWN_FORMAT: String
  val AN_EXCEPTION_WAS_THROWN: String
  val ARE_YOU_KIDDING_ME: String
  val ARTISTS_FS: String
  val A_SMALL_GAME_IN_GUESSING_A_CHARACTER_FD: String
  val A_SMALL_GAME_IN_GUESSING_A_CHARACTER: String
  val A_WINNER_IS_YOU: String
  val ATTACH_A_COMPLETE_LIST_OF_TAGS_TO_REPLY: String
  val BAKA_FD: String
  val BOT_CONTROL_AND_ADMINISTRATION: String
  val BOT_CONTROL_FD: String
  val CHAOMIAN_BAKA: String
  val CHARACTERS_FS: String
  val CHECK_PROXY_AVAILABLE: String
  val CONFIGURATION_HANDLING_FV_FS: String
  val COPYRIGTHS_FS: String
  val CREATING_A_SESSION_FV_FS: String
  val CURRENT_CONFIGURATION_FS: String
  val DELETE_QUOTED_MESSAGE: String
  val DIFFERENT_COMMANDS_FORMAT: String
  val DISPLAY_LIST_OF_SUPPORTED_COMMANDS: String
  val DISPLAY_THIS_HELP_FD: String
  val DISPLAY_THIS_HELP: String
  val DOWNLOAD_AND_ATTACH_IMAGE: String
  val EVERYTHING_IS_BROKEN: String
  val EXAMPLES_OF_USAGE_FS: String
  val FETCH_FROM_DANBOORU_IF_POSSIBLE_FS: String
  val FETCH_FROM_DANBOORU_OR_GELBOORU_IF_POSSIBLE_FS: String
  val FETCH_FROM_DANBOORU_WITH_SIMILARITY_50_FS: String
  val FETCH_IMAGE_AND_ATTACH_FD: String
  val FETCH_IMAGE_AS_DOCUMENT: String
  val FETCH_IMAGE_BY_INDEX_FS: String
  val FETCH_IMAGE_BY_INDEX: String
  val FETCH_IMAGE_BY_SPECIFIED_INDEX: String
  val FETCH_IMAGE_FROM_BOORU_USING_IQDB_ORG: String
  val FETCH_IMAGE_FROM_PIXIV_USING_SAUCENAO: String
  val FETCH_WITH_SIMILARITY_50_FS: String
  val FIND_IMAGE_WITH_IQDB_FD: String
  val FIND_IMAGE_WITH_REVERSE_FD: String
  val FIND_IMAGE_WITH_SAUCENAO_FD: String
  val GET_INFORMATION_ABOUT_QUOTED_USER_OR_YOURSELF: String
  val HANDLING_THE_SESSION_FV_FS: String
  val HERE_ARE_THE_IMAGES_I_FOUND_FS: String
  val I_DONT_KNOW_HOW: String
  val IMAGE_REQUEST_FV_FS: String
  val INVALID_ARGUMENT_FS: String
  val INVALID_LANGUAGE: String
  val INVALID_LOCALE_LIST_FORMAT: String
  val INVALID_SERVER_RESPONSE: String
  val IT_WORKS: String
  val LOCALE_INSTALLED: String
  val LIST_OF_SUPPORTED_COMMANDS_FS: String
  val MESSAGE_SENT: String
  val NEXT_TAG_FS: String
  val NO_IMAGES_FOUND_DUE_TO_EXCEPTION_THROWN_FS: String
  val NO_IMAGES_FOUND_FS: String
  val NOT_PARSED_FS: String
  val NOW_REPLY_ME_WITH_FORMAT: String
  val ONLY_ADMINISTRATOR_CAN_DO_IT: String
  val PERMISSION_IS_ALREADY_GRANTED: String
  val PLEASE_REPLY_TO_MESSAGE_WITH_IMAGE_FORMAT: String
  val PRINT_ALL_SUPPORTED_BOORU_SERVICES: String
  val PRINT_EXAMPLES_OF_USAGE: String
  val PROXY_IS_NOT_PRESENT: String
  val QUERY_IMAGE_WITH_TAGS_FS: String
  val QUERY_LIST_OF_IMAGES_FS: String
  val QUERY_LIST_OF_IMAGES_WITHOUT_RESULT: String
  val RATE_IMAGE_FD: String
  val RATE_IMAGE_USING_EVERYPIXEL_COM: String
  val RATING_REQUEST_FV_FS: String
  val READY: String
  val REQUEST_PERMISSION_TO_INTERACT_WITH_BOT: String
  val RESET_ALL_DEFAULT_ARGUMENTS_FORMAT: String
  val RESET_CONFIGURATION_FS: String
  val RESTART_PROXY: String
  val RESULTS_FS: String
  val SEARCH_IMAGE_USING_REVERSE_SEARCH_ENGINES: String
  val SENDING_THE_MESSAGE_FL_FS: String
  val SEND_MESSAGE_FROM_BOT: String
  val SET_DEFAULT_ARGUMENTS_FOR_USER_FORMAT: String
  val SET_LOCALE_FOR_THIS_CHAT: String
  val SET_MINIMUM_ALLOWED_SIMILARITY_FOR_FOUND_IMAGES: String
  val SET_PRIORITY_FOR_BOORU_SERVICES: String
  val SHOW_UPTIME: String
  val SOMETHING_WENT_WRONG: String
  val SORRY_MY_CONFIGURATION_DOESNT_ALLOW_ME_TO_DO_IT: String
  val SUPPORTED_BOORU_SERVICES_FS: String
  val TAGS_FS: String
  val TARGET_CHAT_ID_OR_ALIAS_FOR_FORMAT: String
  val TRANSLATION_REQUEST_FV_FS: String
  val UNABLE_TO_FETCH_TELEGRAM_FILE: String
  val UNABLE_TO_FETCH_THE_FILE_BY_URL: String
  val UNKNOWN_COMMAND_TYPE_TO_VIEW_HELP_FORMAT: String
  val UPDATE_CONFIGURATION_FS: String
  val VIEW_CONFIGURATION_FS: String
  val WHO_ARE_YOU_FD: String
  val YOU_ARE_NOT_PERMITTED_CONTACT_OWNER_FORMAT: String
  val YOU_ARE_WRONG: String
  val YOU_CAN_VIEW_A_HELP_FORMAT: String
}

object Locale {
  val locales: List[Locale] = List(English, Russian)

  def get(name: String): Option[Locale] = locales.find(_.name == name)

  object English extends Locale {
    override val name: String = "en"

    override val A_LIST_OF_TAGS_TO_PUZZLE_A_CHARACTER: String =
      "A list of tags to puzzle a character."
    override val ADDITIONAL_RESULTS_FS: String =
      "Additional results"
    override val AN_EXCEPTION_WAS_THROWN_FORMAT: String =
      "An exception was thrown during %s."
    override val AN_EXCEPTION_WAS_THROWN: String =
      "An exception was thrown."
    override val ARE_YOU_KIDDING_ME: String =
      "Are you kidding me?"
    override val ARTISTS_FS: String =
      "Artists"
    override val A_SMALL_GAME_IN_GUESSING_A_CHARACTER_FD: String =
      "a small game in guessing a character"
    override val A_SMALL_GAME_IN_GUESSING_A_CHARACTER: String =
      "A small game in guessing a character by \\*booru tags."
    override val A_WINNER_IS_YOU: String =
      "A winner is you!"
    override val ATTACH_A_COMPLETE_LIST_OF_TAGS_TO_REPLY: String =
      "Attach a complete list of tags to reply."
    override val BAKA_FD: String =
      "baka baka"
    override val BOT_CONTROL_AND_ADMINISTRATION: String =
      "Bot control and administration."
    override val BOT_CONTROL_FD: String =
      "bot control"
    override val CHAOMIAN_BAKA: String =
      "Chaomian baka."
    override val CHARACTERS_FS: String =
      "Characters"
    override val CHECK_PROXY_AVAILABLE: String =
      "Check proxy available."
    override val CONFIGURATION_HANDLING_FV_FS: String =
      "configuration handling"
    override val COPYRIGTHS_FS: String =
      "Copyrights"
    override val CREATING_A_SESSION_FV_FS: String =
      "creating a session"
    override val CURRENT_CONFIGURATION_FS: String =
      "Current configuration"
    override val DELETE_QUOTED_MESSAGE: String =
      "Delete quoted message."
    override val DIFFERENT_COMMANDS_FORMAT: String =
      "%s and %s are different commands!"
    override val DISPLAY_LIST_OF_SUPPORTED_COMMANDS: String =
      "Display list of supported commands."
    override val DOWNLOAD_AND_ATTACH_IMAGE: String =
      "Download and attach remote image. " +
      "Useful when you want to convert document to image or download image by URL."
    override val DISPLAY_THIS_HELP_FD: String =
      "display this help"
    override val DISPLAY_THIS_HELP: String =
      "Display this help."
    override val EVERYTHING_IS_BROKEN: String =
      "Everything is broken!"
    override val EXAMPLES_OF_USAGE_FS: String =
      "Examples of usage"
    override val FETCH_FROM_DANBOORU_IF_POSSIBLE_FS: String =
      "Fetch first image from danbooru if possible"
    override val FETCH_FROM_DANBOORU_OR_GELBOORU_IF_POSSIBLE_FS: String =
      "Fetch first image from danbooru or gelbooru if possible"
    override val FETCH_FROM_DANBOORU_WITH_SIMILARITY_50_FS: String =
      "Fetch first image from danbooru with similarity >= 50%"
    override val FETCH_IMAGE_AND_ATTACH_FD: String =
      "fetch image and attach it to message"
    override val FETCH_IMAGE_AS_DOCUMENT: String =
      "Fetch image as document in original quality."
    override val FETCH_IMAGE_BY_INDEX_FS: String =
      "Fetch image by index"
    override val FETCH_IMAGE_BY_INDEX: String =
      "Fetch image by index. Only available when I've already found anything."
    override val FETCH_IMAGE_BY_SPECIFIED_INDEX: String =
      "Fetch image by specified index."
    override val FETCH_IMAGE_FROM_BOORU_USING_IQDB_ORG: String =
      "Fetch image from \\*booru using iqdb.org."
    override val FETCH_IMAGE_FROM_PIXIV_USING_SAUCENAO: String =
      "Fetch image from from pixiv.net using saucenao.com."
    override val FETCH_WITH_SIMILARITY_50_FS: String =
      "Fetch first image with similarity >= 50%"
    override val FIND_IMAGE_WITH_IQDB_FD: String =
      "find image with iqdb"
    override val FIND_IMAGE_WITH_REVERSE_FD: String =
      "find image with reverse search engines"
    override val FIND_IMAGE_WITH_SAUCENAO_FD: String =
      "find image with saucenao"
    override val GET_INFORMATION_ABOUT_QUOTED_USER_OR_YOURSELF: String =
      "Get information about quoted user or yourself."
    override val HANDLING_THE_SESSION_FV_FS: String =
      "handling the session"
    override val HERE_ARE_THE_IMAGES_I_FOUND_FS: String =
      "Here are the images I found"
    override val I_DONT_KNOW_HOW: String =
      "I don't know how!"
    override val IMAGE_REQUEST_FV_FS: String =
      "image request"
    override val INVALID_ARGUMENT_FS: String =
      "Invalid argument"
    override val INVALID_LANGUAGE: String =
      "Invalid language."
    override val INVALID_LOCALE_LIST_FORMAT: String =
      "Invalid locale: %s.\nSupported locales: %s."
    override val INVALID_SERVER_RESPONSE: String =
      "Invalid server response."
    override val IT_WORKS: String =
      "It works!"
    override val LOCALE_INSTALLED: String =
      "Locale installed!"
    override val LIST_OF_SUPPORTED_COMMANDS_FS: String =
      "List of supported commands"
    override val MESSAGE_SENT: String =
      "Message sent."
    override val NEXT_TAG_FS: String =
      "Next tag"
    override val NO_IMAGES_FOUND_DUE_TO_EXCEPTION_THROWN_FS: String =
      "No images found (due to exception thrown)"
    override val NO_IMAGES_FOUND_FS: String =
      "No images found"
    override val NOT_PARSED_FS: String =
      "Not parsed"
    override val NOW_REPLY_ME_WITH_FORMAT: String =
      "Now reply me with %s!"
    override val ONLY_ADMINISTRATOR_CAN_DO_IT: String =
      "Only administrator can do it."
    override val PERMISSION_IS_ALREADY_GRANTED: String =
      "Permission is already granted!"
    override val PLEASE_REPLY_TO_MESSAGE_WITH_IMAGE_FORMAT: String =
      "Please reply to message with image or send image with command in caption.\n\n" +
      "Remember I can't see other bots' messages even when you reply them!\n\n" +
      "Type %s for more information."
    override val PRINT_ALL_SUPPORTED_BOORU_SERVICES: String =
      "Print all supported \\*booru services."
    override val PRINT_EXAMPLES_OF_USAGE: String =
      "Print examples of usage."
    override val PROXY_IS_NOT_PRESENT: String =
      "Proxy is not present."
    override val QUERY_IMAGE_WITH_TAGS_FS: String =
      "Query image with tags"
    override val QUERY_LIST_OF_IMAGES_FS: String =
      "Query list of images"
    override val QUERY_LIST_OF_IMAGES_WITHOUT_RESULT: String =
      "Query list of images without result."
    override val RATE_IMAGE_FD: String =
      "rate image"
    override val RATE_IMAGE_USING_EVERYPIXEL_COM: String =
      "Rate image using everypixel.com."
    override val RATING_REQUEST_FV_FS: String =
      "rating request"
    override val READY: String =
      "Ready!"
    override val REQUEST_PERMISSION_TO_INTERACT_WITH_BOT: String =
      "Request permission to interact with bot."
    override val RESET_ALL_DEFAULT_ARGUMENTS_FORMAT: String =
      "Reset all default arguments. Can be used with %s argument only."
    override val RESET_CONFIGURATION_FS: String =
      "Reset configuration"
    override val RESTART_PROXY: String =
      "Restart proxy."
    override val RESULTS_FS: String =
      "Results"
    override val SEARCH_IMAGE_USING_REVERSE_SEARCH_ENGINES: String =
      "Search image using reverse search engines."
    override val SENDING_THE_MESSAGE_FL_FS: String =
      "sending the message"
    override val SEND_MESSAGE_FROM_BOT: String =
      "Send message from bot."
    override val SET_DEFAULT_ARGUMENTS_FOR_USER_FORMAT: String =
      "Set default arguments for user. Specified %s and %s arguments will be stored as default."
    override val SET_LOCALE_FOR_THIS_CHAT: String =
      "Set locale for this chat."
    override val SET_MINIMUM_ALLOWED_SIMILARITY_FOR_FOUND_IMAGES: String =
      "Set minimum allowed similarity for found images."
    override val SET_PRIORITY_FOR_BOORU_SERVICES: String =
      "Set priority for \\*booru services."
    override val SHOW_UPTIME: String =
      "Show uptime."
    override val SOMETHING_WENT_WRONG: String =
      "Something went wrong!"
    override val SORRY_MY_CONFIGURATION_DOESNT_ALLOW_ME_TO_DO_IT: String =
      "Sorry, my configuration doesn't allow me to do it!"
    override val SUPPORTED_BOORU_SERVICES_FS: String =
      "Supported \\*booru services"
    override val TAGS_FS: String =
      "Tags"
    override val TARGET_CHAT_ID_OR_ALIAS_FOR_FORMAT: String =
      "Target chat ID or alias for %s."
    override val TRANSLATION_REQUEST_FV_FS: String =
      "translation request"
    override val UNABLE_TO_FETCH_TELEGRAM_FILE: String =
      "Unable to fetch Telegram file."
    override val UNABLE_TO_FETCH_THE_FILE_BY_URL: String =
      "Unable to fetch the file by URL."
    override val UNKNOWN_COMMAND_TYPE_TO_VIEW_HELP_FORMAT: String =
      "Unknown command.\nType %s to view help."
    override val UPDATE_CONFIGURATION_FS: String =
      "Update configuration"
    override val VIEW_CONFIGURATION_FS: String =
      "View configuration"
    override val WHO_ARE_YOU_FD: String =
      "who are you?"
    override val YOU_ARE_NOT_PERMITTED_CONTACT_OWNER_FORMAT: String =
      "You are not permitted to use this bot.\nPlease, contact bot owner using %s command."
    override val YOU_ARE_WRONG: String =
      "You are wrong!"
    override val YOU_CAN_VIEW_A_HELP_FORMAT: String =
      "You can view a help for each command using %s."
  }

  object Russian extends Locale {
    override val name: String = "ru"

    override val A_LIST_OF_TAGS_TO_PUZZLE_A_CHARACTER: String =
      "Список тегов чтобы загадать персонажа."
    override val ADDITIONAL_RESULTS_FS: String =
      "Дополнительные результаты"
    override val AN_EXCEPTION_WAS_THROWN_FORMAT: String =
      "При %s произошла ошибка."
    override val AN_EXCEPTION_WAS_THROWN: String =
      "Произошла ошибка."
    override val ARE_YOU_KIDDING_ME: String =
      "Ты шутишь что ли?"
    override val ARTISTS_FS: String =
      "Художники"
    override val A_SMALL_GAME_IN_GUESSING_A_CHARACTER_FD: String =
      "небольшая игра в угадывание персонажа"
    override val A_SMALL_GAME_IN_GUESSING_A_CHARACTER: String =
      "Небольшая игра в угадывание персонажа по \\*booru тегам."
    override val A_WINNER_IS_YOU: String =
      "A winner is you!"
    override val ATTACH_A_COMPLETE_LIST_OF_TAGS_TO_REPLY: String =
      "Приложить полный список тегов к ответу."
    override val BAKA_FD: String =
      "baka baka"
    override val BOT_CONTROL_AND_ADMINISTRATION: String =
      "Управление и администрирование бота."
    override val BOT_CONTROL_FD: String =
      "управление ботом"
    override val CHAOMIAN_BAKA: String =
      "Chaomian baka."
    override val CHARACTERS_FS: String =
      "Персонажи"
    override val CHECK_PROXY_AVAILABLE: String =
      "Проверить работу прокси."
    override val CONFIGURATION_HANDLING_FV_FS: String =
      "обработке конфигурации"
    override val COPYRIGTHS_FS: String =
      "Произведения"
    override val CREATING_A_SESSION_FV_FS: String =
      "создании сессии"
    override val CURRENT_CONFIGURATION_FS: String =
      "Текущая конфигурация"
    override val DELETE_QUOTED_MESSAGE: String =
      "Удалить процитированное сообщение."
    override val DIFFERENT_COMMANDS_FORMAT: String =
      "%s и %s — разные команды!"
    override val DISPLAY_LIST_OF_SUPPORTED_COMMANDS: String =
      "Отображение списка поддерживаемых команд."
    override val DISPLAY_THIS_HELP_FD: String =
      "показать эту справку"
    override val DISPLAY_THIS_HELP: String =
      "Показать эту справку."
    override val DOWNLOAD_AND_ATTACH_IMAGE: String =
      "Загрузка и прикрепление картинки. " +
      "Позволяет сконвертировать документ в картинку или загрузить картинку по ссылке."
    override val EVERYTHING_IS_BROKEN: String =
      "Всё сломано!"
    override val EXAMPLES_OF_USAGE_FS: String =
      "Примеры использования"
    override val FETCH_FROM_DANBOORU_IF_POSSIBLE_FS: String =
      "Получить первую картинку с danbooru если возможно"
    override val FETCH_FROM_DANBOORU_OR_GELBOORU_IF_POSSIBLE_FS: String =
      "Получить первую картинку с danbooru или gelbooru если возможно"
    override val FETCH_FROM_DANBOORU_WITH_SIMILARITY_50_FS: String =
      "Получить первую картинку с danbooru со сходством >= 50%"
    override val FETCH_IMAGE_AND_ATTACH_FD: String =
      "получить картинку и прикрепить к сообщению"
    override val FETCH_IMAGE_AS_DOCUMENT: String =
      "Получить картинку в виде документа в исходном качестве."
    override val FETCH_IMAGE_BY_INDEX_FS: String =
      "Получить картинку по номеру"
    override val FETCH_IMAGE_BY_INDEX: String =
      "Получить картинку по номеру. Доступно только когда я уже что-то нашла."
    override val FETCH_IMAGE_BY_SPECIFIED_INDEX: String =
      "Получить картинку по указанному номеру."
    override val FETCH_IMAGE_FROM_BOORU_USING_IQDB_ORG: String =
      "Получуние картинки с \\*booru используя iqdb.org."
    override val FETCH_IMAGE_FROM_PIXIV_USING_SAUCENAO: String =
      "Получение картинки с pixiv.net используя saucenao.com."
    override val FETCH_WITH_SIMILARITY_50_FS: String =
      "Получить картинку со сходством >= 50%"
    override val FIND_IMAGE_WITH_IQDB_FD: String =
      "найти картинку через iqdb"
    override val FIND_IMAGE_WITH_REVERSE_FD: String =
      "найти картинку через сервисы обратного поиска"
    override val FIND_IMAGE_WITH_SAUCENAO_FD: String =
      "найти картинку через saucenao"
    override val GET_INFORMATION_ABOUT_QUOTED_USER_OR_YOURSELF: String =
      "Получение информации о цитируемом пользователе или о себе."
    override val HANDLING_THE_SESSION_FV_FS: String =
      "обработке сессии"
    override val HERE_ARE_THE_IMAGES_I_FOUND_FS: String =
      "Вот картинки, которые я нашла"
    override val I_DONT_KNOW_HOW: String =
      "Я не знаю как!"
    override val IMAGE_REQUEST_FV_FS: String =
      "запросе картинки"
    override val INVALID_ARGUMENT_FS: String =
      "Аргумент инвалид"
    override val INVALID_LANGUAGE: String =
      "Некорректный язык."
    override val INVALID_LOCALE_LIST_FORMAT: String =
      "Некорректный язык: %s.\nПоддерживаемые языки: %s."
    override val INVALID_SERVER_RESPONSE: String =
      "Некорректный ответ сервера."
    override val IT_WORKS: String =
      "Оно работает!"
    override val LOCALE_INSTALLED: String =
      "Язык установлен!"
    override val LIST_OF_SUPPORTED_COMMANDS_FS: String =
      "Список поддерживаемых команд"
    override val MESSAGE_SENT: String =
      "Сообщение отправлено."
    override val NEXT_TAG_FS: String =
      "Следующий тег"
    override val NO_IMAGES_FOUND_DUE_TO_EXCEPTION_THROWN_FS: String =
      "Картинки не найдены (потому что произошла ошибка)"
    override val NO_IMAGES_FOUND_FS: String =
      "Картинки не найдены"
    override val NOT_PARSED_FS: String =
      "Не распарсила"
    override val NOW_REPLY_ME_WITH_FORMAT: String =
      "Теперь ответь мне, написав %s!"
    override val ONLY_ADMINISTRATOR_CAN_DO_IT: String =
      "Вы не модератор, чтобы указывать мне!"
    override val PERMISSION_IS_ALREADY_GRANTED: String =
      "Разрешение уже получено!"
    override val PLEASE_REPLY_TO_MESSAGE_WITH_IMAGE_FORMAT: String =
      "Пожалуйста, ответь на сообщение с картинкой или отправь сообщение с командой в подписи.\n\n" +
      "Помни, я не могу видеть сообщения других ботов даже когда ты им отвечаешь!\n\n" +
      "Напиши %s для получения дополнительной информации."
    override val PRINT_ALL_SUPPORTED_BOORU_SERVICES: String =
      "Вывести поддерживаемые \\*booru сервисы."
    override val PRINT_EXAMPLES_OF_USAGE: String =
      "Вывести примеры использования."
    override val PROXY_IS_NOT_PRESENT: String =
      "Прокси не используется."
    override val QUERY_IMAGE_WITH_TAGS_FS: String =
      "Запросить картинку с тегами"
    override val QUERY_LIST_OF_IMAGES_FS: String =
      "Запросить список картинок"
    override val QUERY_LIST_OF_IMAGES_WITHOUT_RESULT: String =
      "Запросить список картинок без результата."
    override val RATE_IMAGE_FD: String =
      "оценить картинку"
    override val RATE_IMAGE_USING_EVERYPIXEL_COM: String =
      "Оценка картинки через everypixel.com."
    override val RATING_REQUEST_FV_FS: String =
      "запросе оценки"
    override val READY: String =
      "Готово!"
    override val REQUEST_PERMISSION_TO_INTERACT_WITH_BOT: String =
      "Запросить разрешение на взаимодействие с ботом."
    override val RESET_ALL_DEFAULT_ARGUMENTS_FORMAT: String =
      "Сбросить аргументы по умолчанию. Может быть использовано только с %s."
    override val RESET_CONFIGURATION_FS: String =
      "Сбросить конфигурацию"
    override val RESTART_PROXY: String =
      "Перезапустить прокси."
    override val RESULTS_FS: String =
      "Результаты"
    override val SEARCH_IMAGE_USING_REVERSE_SEARCH_ENGINES: String =
      "Поиск картинки через сервисы обратного поиска."
    override val SENDING_THE_MESSAGE_FL_FS: String =
      "отправке сообщения"
    override val SEND_MESSAGE_FROM_BOT: String =
      "Отправить сообщение от бота."
    override val SET_DEFAULT_ARGUMENTS_FOR_USER_FORMAT: String =
      "Установить аргументы по умолчанию для пользователя. Указанные аргументы %s и %s будут сохранены."
    override val SET_LOCALE_FOR_THIS_CHAT: String =
      "Установить язык для этого чата."
    override val SET_MINIMUM_ALLOWED_SIMILARITY_FOR_FOUND_IMAGES: String =
      "Установить минимальное сходство для найденых картинок."
    override val SET_PRIORITY_FOR_BOORU_SERVICES: String =
      "Установить приоритет для \\*booru сервисов."
    override val SHOW_UPTIME: String =
      "Показать время работы."
    override val SOMETHING_WENT_WRONG: String =
      "Что-то пошло не так!"
    override val SORRY_MY_CONFIGURATION_DOESNT_ALLOW_ME_TO_DO_IT: String =
      "Прости, мои настройки не позволят мне это сделать!"
    override val SUPPORTED_BOORU_SERVICES_FS: String =
      "Поддерживаемые \\*booru сервисы"
    override val TAGS_FS: String =
      "Теги"
    override val TARGET_CHAT_ID_OR_ALIAS_FOR_FORMAT: String =
      "ID или псевдоним целевого чата для %s."
    override val TRANSLATION_REQUEST_FV_FS: String =
      "запросе перевода"
    override val UNABLE_TO_FETCH_TELEGRAM_FILE: String =
      "Не удалось получить файл Telegram."
    override val UNABLE_TO_FETCH_THE_FILE_BY_URL: String =
      "Не удалось получить файл по URL."
    override val UNKNOWN_COMMAND_TYPE_TO_VIEW_HELP_FORMAT: String =
      "Неизвестная команда.\nВведи %s чтобы посмотреть справку."
    override val UPDATE_CONFIGURATION_FS: String =
      "Обновить конфигурацию"
    override val VIEW_CONFIGURATION_FS: String =
      "Посмотреть конфигурацию"
    override val WHO_ARE_YOU_FD: String =
      "кто ты?"
    override val YOU_ARE_NOT_PERMITTED_CONTACT_OWNER_FORMAT: String =
      "У тебя нет разрешения пользоваться ботом.\nПожалуйста, напиши владельцу бота используя команду %s."
    override val YOU_ARE_WRONG: String =
      "Не угадал!"
    override val YOU_CAN_VIEW_A_HELP_FORMAT: String =
      "Ты можешь посмотреть справку для каждой команды используя %s."
  }
}
