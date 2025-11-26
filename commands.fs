module Commands

open Domain
open Utils
open Errors
open Localization

type Command =
    | Start
    | SendChangeSkin

type CommandUpdate = Id * Id * Command

type CommandInfo =
    { text: string
      canShowDescription: bool
      isMain: bool
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
        isMain = false
        canShowDescription = false }
      { text = "changeSkin"
        description = ru "Позволяет сменить фон цитаты"
        command = SendChangeSkin
        supportedChatType = all
        isMain = true
        canShowDescription = true } ]


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

let formatCommandText chatType botName command =
    $"/{commandName chatType botName command}"

let commandText chatType botName command =
    let text = formatCommandText chatType botName command.text
    let description = to_ru command.description
    $"{text} - {description}"

let mainCommandsDescription botName chatType =
    commands
    |> List.filter (fun command ->
        command.canShowDescription
        && command.supportedChatType |> List.contains chatType)
    |> List.filter (fun command -> command.isMain)
    |> List.map (fun command -> commandText chatType botName command)
    |> join "\n"
