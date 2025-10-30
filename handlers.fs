module Handlers

open System
open System.IO
open Funogram.Api
open Funogram.Telegram.Bot
open Funogram.Types
open Skin
open SimpleSkins
open Utils
open Database

type TelegramUpdate = Funogram.Telegram.Types.Update
type TelegramMessage = Funogram.Telegram.Types.Message
type TelegramChatType = Funogram.Telegram.Types.ChatType
type TelegramChat = Funogram.Telegram.Types.Chat
type TelegramReplyParams = Funogram.Telegram.Types.ReplyParameters
type TelegramChatId = Funogram.Telegram.Types.ChatId

let processResultWithValue (result: Async<Result<'a, ApiResponseError>>) =
    async {
        let! result = result

        match result with
        | Ok _ -> ignore ()
        | Error e -> printfn "Server error: %s" e.Description

        return result
    }

let getImagePath () =
    let tempFile = DateTime.Now.ToFileTimeUtc().ToString() + ".png"
    Path.Combine(Environment.CurrentDirectory, "./temp/" + tempFile)

let getInputFile path =
    let fileStream = new FileStream(path, FileMode.Open, FileAccess.Read)
    Funogram.Telegram.Types.InputFile.File(path, fileStream)

let dispose (f: Funogram.Telegram.Types.InputFile) result =
    async {
        let! res = result

        match f with
        | Funogram.Telegram.Types.InputFile.File(p, f) ->
            match File.Exists(p) with
            | true -> File.Delete(p)
            | _ -> ()

            f.Dispose()
        | _ -> ()

        return result
    }

type TextUpdate =
    { skin: string -> Result<Skin, string>
      chatId: int64
      text: string
      replyMessageId: int64 }

type SuperTextUpdate =
    { topicId: int64
      textUpdate: TextUpdate }

type ResolvedUpdate = TextUpdate

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
    let parts =
        text.Split([| " " |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

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
            return
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
                    { skin = skin
                      chatId = chatId
                      replyMessageId = replyMessage.MessageId
                      text = replyText }
            | _ -> ignore ()
        | _ -> ignore ()
    }

let sendPhoto (update: ResolvedUpdate) chatId inputFile =
    let sendler = Funogram.Telegram.Api.sendPhoto chatId inputFile ""
    let chatId = TelegramChatId.Int chatId
    let replyInfo = TelegramReplyParams.Create(update.replyMessageId, chatId)

    { sendler with
        ReplyParameters = Some replyInfo }

let sendMessage context (update: ResolvedUpdate) =
    let imagePath = getImagePath ()
    let textUpdate = update

    match textUpdate.text |> generateQuote imagePath update.skin with
    | Error e -> Error e
    | Ok _ ->
        let inputFile = getInputFile imagePath

        sendPhoto update textUpdate.chatId inputFile
        |> api context.Config
        |> processResultWithValue
        |> dispose inputFile
        |> Async.Ignore
        |> Async.Start

        Ok 0

let update (repository: ChatRepository) context =
    printfn $"Get update: {context.Update.UpdateId}"

    match resolveUpdate repository context with
    | Some resolved ->
        match sendMessage context resolved with
        | Ok _ -> printfn "Update process ok"
        | Error e -> printfn $"Error: {e}"
    | None -> printfn "Message in not for proccessing"
