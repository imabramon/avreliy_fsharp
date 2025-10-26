module Handlers

open System
open System.IO
open Funogram.Api
open Funogram.Telegram.Bot
open Funogram.Types
open Skin
open Avrelii
open Utils

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
    { chatId: int64
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

let resolveUpdate (context: UpdateContext) =
    maybe {
        let! message = context.Update.Message
        let! text = message.Text
        let chat = message.Chat
        let chatId = chat.Id
        let chatType = chat.Type

        match chatType, chat with
        | SingleChat _ ->
            return
                { chatId = chatId
                  text = text
                  replyMessageId = message.MessageId }
        | GroupChat _ ->
            let! replyMessage = message.ReplyToMessage
            let! replyText = replyMessage.Text
            let! botName = context.Me.Username

            match text = $"@{botName}" with
            | true ->
                return
                    { chatId = chatId
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

    match textUpdate.text |> generateQuote imagePath avrelii with
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

let update context =
    printfn $"Get update: {context.Update.UpdateId}"

    match resolveUpdate context with
    | Some resolved ->
        match sendMessage context resolved with
        | Ok _ -> printfn "Update process ok"
        | Error e -> printfn $"Error: {e}"
    | None -> printfn "Message in not for proccessing"
