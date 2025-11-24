module Routing

open System
open Funogram.Telegram.Bot

open Domain
open Utils
open Maybe
open Handlers
open Errors
open Database
open SimpleSkins
open Result

type ResolvedUpdate =
    | TextUpdate of TextUpdate
    | CommandUpdate of CommandUpdate
    | QueryUpdate of TCallbackQuery

type ResolvedChatType =
    | SingleChat
    | GroupChat

let resolveChatType (chat: TChat) =
    let chatType = chat.Type

    match chatType with
    | TChatType.Private -> Ok SingleChat
    | TChatType.SuperGroup when chat.IsForum = Some true -> Ok GroupChat
    | TChatType.SuperGroup -> Ok GroupChat
    | TChatType.Group -> Ok GroupChat
    | _ -> logError "Unsupported chat type"

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

let (|HasCommand|HasNoCommand|) (text: string) =
    let parts = toWords text

    match parts with
    | first :: _ when first.StartsWith '/' -> HasCommand first
    | _ -> HasNoCommand

let resolveCommand commandText (chatType: ResolvedChatType) =
    match commandText, chatType with
    | "/start", SingleChat -> Ok Start
    | "/changeSkin", _ -> Ok SendChangeSkin
    | _ -> sendError "Я не поддерживаю такие комманды"

let getGroupText (botName: string) (text: string) =
    let resolvedBotTag = $"@{botName}"

    if text.Contains resolvedBotTag then
        Ok(text.Replace(resolvedBotTag, ""))
    else
        logError "Group message without bot tag"

let resolveMessageText (original: UpdateContext) chatType (message: TMessage) =
    match message.Text with
    | Some text ->
        match chatType with
        | SingleChat -> Ok text
        | GroupChat ->
            match original.Me.Username with
            | Some botName -> getGroupText botName text
            | None -> logError "Cant validate group message without botname"
    | None ->
        match chatType with
        | SingleChat -> sendError "Я не поддерживаю такие сообщения"
        | GroupChat -> logError "Group Message is not for proccessing"

let resolveReplyMessage (message: TMessage) =
    match message.ReplyToMessage with
    | Some message -> Ok message
    | None -> sendError "Чтобы сгенерировать цитату, ответьте на сообщение и тегните меня"

let resolveUpdateByMessage
    repository
    (original: UpdateContext)
    (message: TMessage)
    : Result<ResolvedUpdate, ErrorExternal> =
    result {
        let chat = message.Chat
        let chatId = chat.Id
        let! chatType = resolveChatType chat
        let! text = resolveMessageText original chatType message
        let messageId = message.MessageId
        let skinNameByChatId = repository.get chatId |> noneIfError
        let chatSkin = skinByOpionName skinNameByChatId
        let defaultSkin = chatSkin |> withDefault avrelii

        match text with
        | HasCommand command ->
            let! command = resolveCommand command chatType
            return CommandUpdate(chatId, messageId, command)
        | _ ->
            match chatType with
            | SingleChat ->
                return
                    TextUpdate
                        { skin = defaultSkin
                          chatId = chatId
                          replyMessageId = messageId
                          text = text }
            | GroupChat ->
                let! replyMessage = resolveReplyMessage message
                let! replyText = resolveMessageText original SingleChat replyMessage
                let selectedSkin = skinByName text |> withDefault defaultSkin

                return
                    TextUpdate
                        { skin = selectedSkin
                          chatId = chatId
                          replyMessageId = replyMessage.MessageId
                          text = replyText }
    }

let resolveUpdate repository (context: UpdateContext) : Result<ResolvedUpdate, ErrorExternal> =
    match resolveSupportedUpdate context with
    | Message m -> resolveUpdateByMessage repository context m
    | CallbackQuery q -> Ok(QueryUpdate q)
    | _ -> logError "Unsupported update to resolve"

let proccessUpdate repository context : Result<unit, ErrorExternal> =
    result {
        printfn $"Get update: {context.Update.UpdateId}"

        let! update = resolveUpdate repository context

        match update with
        | TextUpdate update -> do! sendQuote context update
        | CommandUpdate command -> proccessCommand context command
        | QueryUpdate query -> do! proccessQuery repository context query

        return ()
    }

let checkTime (start: DateTime) update =
    match getTime update with
    | Some current ->
        let diff = (current - start).TotalSeconds

        if diff >= 0.0 then
            Ok()
        else
            logError $"Time difference is too large: {diff} seconds"
    | None -> logError "Cant get update time"

let validateTime date (context: UpdateContext) =
    let update = resolveSupportedUpdate context

    match update with
    | CallbackQuery _ -> Ok()
    | UnsupportedUpdate -> logError "Unsupported update"
    | _ -> checkTime date update

let validateUpdate config update =
    result {
        do! validateTime config.startDate update
        return ()
    }

type MessageToReply = { chatId: int64; messageId: int64 }

let getMessageToReply (update: UpdateContext) =
    match resolveSupportedUpdate update with
    | Message m ->
        Some
            { chatId = m.Chat.Id
              messageId = m.MessageId }
    | _ -> None

let giveFeedback context messageToReply text =
    sendMessage context messageToReply.chatId messageToReply.messageId text

let update botContext update =
    let messageToReply = getMessageToReply update

    match
        result {
            do! validateUpdate botContext.validation update
            do! proccessUpdate botContext.repository update
        }
    with
    | Ok _ -> ()
    | Error e ->
        match e, messageToReply with
        | PublicError e, Some messageToReply -> giveFeedback update messageToReply e.message
        | _ -> printfn $"Error: {getMessage e}"
