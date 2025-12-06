module Commands

open Domain
open Utils
open Errors
open Localization
open Funogram.Telegram.Types

type Command =
    | Start
    | SendChangeSkin
    | Help
    | Examples

type CommandUpdate =
    { chatId: Id
      messageId: Id
      command: Command
      chatType: ResolvedChatType }

type CommandInfo =
    { text: string
      showInHelp: bool
      showInStart: bool
      description: Localization
      command: Command
      supportedChatType: ResolvedChatType list }

let all = [ SingleChat; GroupChat ]
let onlySingle = [ SingleChat ]
let onlyGroup = [ GroupChat ]

let commands =
    [ { text = "start"
        description = ru "Запускает чат"
        command = Start
        supportedChatType = onlySingle
        showInHelp = false
        showInStart = false }
      { text = "changeskin"
        description = ru "Позволяет сменить фон цитаты"
        command = SendChangeSkin
        supportedChatType = all
        showInHelp = true
        showInStart = true }
      { text = "help"
        description = ru "Список всех комманд"
        command = Help
        supportedChatType = all
        showInHelp = false
        showInStart = false }
      { text = "examples"
        description = ru "Примеры фононов для цитат"
        command = Examples
        supportedChatType = all
        showInHelp = true
        showInStart = false } ]


let commandInfoMap =
    commands
    |> List.collect (fun info -> info.supportedChatType |> List.map (fun chat -> ($"/{info.text}", chat), info))
    |> Map


let getCommandInfo command chat = commandInfoMap.TryFind(command, chat)

let (|HasCommand|HasNoCommand|) (text: string) =
    let parts = toWords text

    match parts with
    | first :: _ when first.StartsWith '/' -> HasCommand first
    | _ -> HasNoCommand

let resolveCommand (commandText: string) (chatType: ResolvedChatType) =
    match getCommandInfo commandText chatType with
    | Some info -> Ok info.command
    | _ -> sendError "Я не поддерживаю такой комманды"

let commandName chatType botName command =
    match chatType with
    | SingleChat -> command
    | GroupChat -> $"{command}@{botName}"

let formatCommand (text: string) =
    let text = text.Trim()

    match text.StartsWith "/" with
    | true -> text.Substring 1
    | false -> text

let useStartCommands command = command.showInStart
let useHelpCommands command = command.showInHelp

let useSelectedChad chatType command =
    command.supportedChatType |> List.contains chatType

let formatCommandText chatType botName command =
    $"/{commandName chatType botName command}"

let commandText chatType botName command =
    let text = formatCommandText chatType botName command.text
    let description = to_ru command.description
    $"{text} - {description}"

let getCommandsDescription filter (botName: string) chatType =
    let useChatType = useSelectedChad chatType

    commands
    |> List.filter useChatType
    |> List.filter filter
    |> List.map (fun command -> commandText chatType botName command)
    |> join "\n"

let startCommandsDescription = getCommandsDescription useStartCommands
let helpCommandsDescription = getCommandsDescription useHelpCommands

let commandInfoToCompletion chatType botName (id: int) command =
    let id = id.ToString()
    let content = InputTextMessageContent.Create(commandText chatType botName command)
    InlineQueryResultArticle.Create("Command", id, to_ru command.description, TextMessageContent content)

let getHelpText commands =
    $"""Список всех комманд:
{commands}"""
