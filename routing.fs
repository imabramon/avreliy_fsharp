module Routing

open System
open Funogram.Telegram.Bot

open Domain
open Utils
open Maybe
open Handlers
open Database
open SimpleSkins
open Result

type ResolvedUpdate =
    | TextUpdate of TextUpdate
    | CommandUpdate of CommandUpdate
    | QueryUpdate of TCallbackQuery

type GroupChatType =
    | Group
    | SuperGroup

type ResolvedChatType =
    | SingleChat
    | GroupChat of GroupChatType

let resolveChatType (chat: TChat) =
    let chatType = chat.Type

    match chatType with
    | TChatType.Private -> Some SingleChat
    | TChatType.SuperGroup when chat.IsForum = Some true -> Some(GroupChat Group)
    | TChatType.SuperGroup -> Some(GroupChat SuperGroup)
    | TChatType.Group -> Some(GroupChat Group)
    | _ -> None

let skinByName name =
    match name with
    | "аврелий"
    | "авр"
    | "Аврелий"
    | "avrelii" -> Some avrelii
    | "стетхем"
    | "Стетхем"
    | "stetham"
    | "стет" -> Some stetham
    | "chad"
    | "чад"
    | "Чад"
    | "Chad" -> Some chad
    | "Джокер"
    | "джокер"
    | "joker" -> Some joker
    | _ -> None

let skinByOpionName name =
    match name with
    | Some name -> skinByName name
    | None -> None

let (|OnMessage|OnCallback|OnEmpty|) (context: UpdateContext) =
    let query = context.Update.CallbackQuery
    let message = context.Update.Message

    match query with
    | Some query -> OnCallback(query)
    | None ->
        match message with
        | None -> OnEmpty
        | Some message ->
            match message.Text with
            | None -> OnEmpty
            | Some text -> OnMessage(message, text)

let (|IsNeedQuote|DontNeed|) (text: string, botName) =
    let parts = toWords text

    let resolvedBotTag = $"@{botName}"
    let checkBotTag tag = resolvedBotTag = tag

    match parts with
    | botTag :: skinName :: [] when checkBotTag botTag ->
        let skin = skinByName skinName
        IsNeedQuote skin
    | botTag :: _ when checkBotTag botTag -> IsNeedQuote None
    | _ -> DontNeed

let (|HasCommand|HasNoCommand|) (text: string) =
    let parts = toWords text

    match parts with
    | first :: _ when first.StartsWith '/' -> HasCommand first
    | _ -> HasNoCommand

let resolveCommand commandText (chatType: ResolvedChatType) =
    match commandText, chatType with
    | "/start", SingleChat -> Some Start
    | "/changeSkin", _ -> Some SendChangeSkin
    | _ -> None


let resolveUpdate (repository: ChatRepository) (context: UpdateContext) =
    maybe {
        match context with
        | OnCallback query -> return QueryUpdate query
        | OnMessage(message, text) ->
            let chat = message.Chat
            let chatId = chat.Id
            let! chatType = resolveChatType chat
            let skinNameByChatId = repository.get chatId |> noneIfError
            let chatSkin = skinByOpionName skinNameByChatId
            let defaultSkin = chatSkin |> withDefault avrelii

            match text with
            | HasCommand command ->
                let! command = resolveCommand command chatType
                return CommandUpdate(chatId, message.MessageId, command)
            | _ ->
                match chatType with
                | SingleChat ->
                    return
                        TextUpdate
                            { skin = defaultSkin
                              chatId = chatId
                              text = text
                              replyMessageId = message.MessageId }
                | GroupChat _ ->
                    let! replyMessage = message.ReplyToMessage
                    let! replyText = replyMessage.Text
                    let! botName = context.Me.Username

                    match text, botName with
                    | IsNeedQuote choisenSkin ->
                        let skin = choisenSkin |> withDefault defaultSkin

                        return
                            TextUpdate
                                { skin = skin
                                  chatId = chatId
                                  replyMessageId = replyMessage.MessageId
                                  text = replyText }
                    | _ -> return! None
        | OnEmpty -> return! None
    }

let proccessUpdate (repository: ChatRepository) context =
    maybe {
        printfn $"Get update: {context.Update.UpdateId}"

        let! update = resolveUpdate repository context

        match update with
        | TextUpdate update ->
            match sendQuote context update with
            | Ok _ -> printfn "Update process ok"
            | Error e -> printfn $"Error: {e}"
        | CommandUpdate command -> proccessCommand context command
        | QueryUpdate query -> proccessQuery repository context query |> ignore
    }
    |> ignore

let checkTime (start: DateTime) update =
    match getTime update with
    | Some current ->
        let diff = (current - start).TotalSeconds

        if diff >= 0.0 then
            Ok()
        else
            Error $"Time difference is too large: {diff} seconds"
    | None -> Error "Cant get update time"

let validateTime date (context: UpdateContext) =
    let update = resolveSupportedUpdate context

    match update with
    | CallbackQuery _ -> Ok()
    | UnsupportedUpdate -> Error "Unsupported update"
    | _ -> checkTime date update

let validateUpdate config update : Result<unit, string> =
    result {
        do! validateTime config.startDate update
        return! Ok()
    }

let update botContext update =
    match validateUpdate botContext.validation update with
    | Error e -> printfn $"Error: {e}"
    | Ok _ -> proccessUpdate botContext.repository update
