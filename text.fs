module Text

let getStartMessage (usageText: string) (helpCommand: string) (mainCommands: string) =
    $"""Приветствую, я Бот цитатник
Я могу генерировать цитаты по запросу.

{usageText}

Мои основные команды:
{mainCommands}
{helpCommand} - Полный список комманд"""

let singleChatUsage = """Чтобы сгенирировать цитату отправьте сообщение в чат"""

let groupChatUsage botName =
    $"""Чтобы сгенирировать цитату:
1. Дайте доступ боту к истории чата в настройках.
2. Ответьте на сообщение, которое хотите процитировать и тегните меня (@{botName})
Так же вы можете после упоминания написать желаемый фон"""

let helpCommand = "/help"

let singleStartMessage = getStartMessage singleChatUsage helpCommand

let groupStartMessage botName =
    getStartMessage (groupChatUsage botName) $"{helpCommand}@{botName}"
