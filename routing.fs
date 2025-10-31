module Routing

open Funogram.Telegram.Bot

open Domain
open Utils
open Maybe
open Handlers
open Database
open SimpleSkins

type Command = Start of Id

type ResolvedUpdate =
    | TextUpdate of TextUpdate
    | Command of Command

type GroupChatType =
    | Group
    | SuperGroup

type ResolvedChatType =
    | SingleChat
    | GroupChat of GroupChatType

let (|SingleChat|GroupChat|OtherChat|) (chatType: TelegramChatType, chat: TelegramChat) =
    match chatType with
    | TelegramChatType.Private -> SingleChat ResolvedChatType.SingleChat
    | TelegramChatType.SuperGroup when chat.IsForum = Some true -> GroupChat SuperGroup
    | TelegramChatType.SuperGroup -> GroupChat Group
    | TelegramChatType.Group -> GroupChat Group
    | _ -> OtherChat chatType

let skinByName name =
    match name with
    | "аврелий"
    | "авр"
    | "Аврелий" -> Some avrelii
    | "стетхем"
    | "Стетхем"
    | "стет" -> Some stetham
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

let (|IsNeedQuote|DontNeed|) (text: string, botName) =
    let parts = toWords text

    let resolvedBotTag = $"@{botName}"
    let checkBotTag tag = resolvedBotTag = tag

    match parts with
    | botTag :: skinName :: [] when checkBotTag botTag ->
        let skin = skinByName skinName
        IsNeedQuote skin
    | botTag :: rest when checkBotTag botTag -> IsNeedQuote None
    | _ -> DontNeed

let resolveUpdate (repository: ChatRepository) (context: UpdateContext) =
    maybe {
        let! message = context.Update.Message
        let! text = message.Text
        let chat = message.Chat
        let chatId = chat.Id
        let chatType = chat.Type
        let skinNameByChatId = repository.get chatId |> noneIfError
        let chatSkin = skinByOpionName skinNameByChatId
        let defaultSkin = chatSkin |> withDefault avrelii
        printfn $"chat id: {chatId}"

        match chatType, chat with
        | SingleChat _ ->
            let parts = toWords text

            match parts with
            | first :: _ when first = "/start" -> return Command(Start chatId)
            | _ ->
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
            | _ -> ignore ()
        | _ -> ignore ()
    }

let (|OnText|OnStart|OnEmpty|) (update: ResolvedUpdate option) =
    match update with
    | None -> OnEmpty
    | Some resolved ->
        match resolved with
        | TextUpdate update -> OnText update
        | Command command ->
            match command with
            | Start chatId -> OnStart chatId

let update (repository: ChatRepository) context =
    printfn $"Get update: {context.Update.UpdateId}"

    match resolveUpdate repository context with
    | OnText update ->
        match sendQuote context update with
        | Ok _ -> printfn "Update process ok"
        | Error e -> printfn $"Error: {e}"
    | OnStart chatId -> sendMessage context chatId "Привет, я бот цитатник"
    | OnEmpty -> printfn "Message in not for proccessing"
