module Handlers

open System
open System.IO
open Funogram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types

open Domain
open Skin
open Utils
open Result
open Errors
open Localization
open AvailableSkins
open Commands
open Text

let getImagePath () =
    let tempFile = DateTime.Now.ToFileTimeUtc().ToString() + ".png"
    Path.Combine(Environment.CurrentDirectory, "./temp/" + tempFile)

let getInputFile path =
    let fileStream = new FileStream(path, FileMode.Open, FileAccess.Read)
    InputFile.File(path, fileStream)

let dispose (f: Funogram.Telegram.Types.InputFile) result =
    async {
        let! res = result

        match f with
        | InputFile.File(p, f) ->
            match File.Exists p with
            | true -> File.Delete p
            | _ -> ()

            f.Dispose()
        | _ -> ()

        return res
    }

type TextUpdate =
    { skin: string -> Result<Skin, ErrorExternal>
      chatId: Id
      text: string
      replyMessageId: Id }

let sendPhoto (update: TextUpdate) chatId inputFile =
    let sendler = Funogram.Telegram.Api.sendPhoto chatId inputFile ""
    let chatId = Int chatId
    let replyInfo = ReplyParameters.Create(update.replyMessageId, chatId)

    { sendler with
        ReplyParameters = Some replyInfo }

let sendQuote context (update: TextUpdate) =
    result {
        let imagePath = getImagePath ()
        let textUpdate = update

        do! generateQuote imagePath update.skin textUpdate.text

        let inputFile = getInputFile imagePath

        sendPhoto update textUpdate.chatId inputFile
        |> api context.Config
        |> dispose inputFile
        |> asyncStart

        return ()
    }


let sendMessage context chatId replyId message =
    Funogram.Telegram.Api.sendMessage chatId message
    |> api context.Config
    |> asyncStart

let sendMessageMarkup context chatId replyId message markup =
    let req = Funogram.Telegram.Api.sendMessageMarkup chatId message markup

    { req with
        ReplyParameters = Some(ReplyParameters.Create(replyId, Int chatId)) }
    |> api context.Config
    |> asyncStart

let SET_SKIN = "setSkin"

let createChaneSkinMarkup skinsInfo =
    skinsInfo
    |> List.map (fun skinInfo ->
        [| InlineKeyboardButton.Create(to_ru skinInfo.localization, callbackData = $"{SET_SKIN},{skinInfo.name}") |])
    |> List.toArray
    |> TInlineKeyboardMarkup.Create
    |> InlineKeyboardMarkup

let sendChangeSkinMessage context chatId messageId =
    availabelSkins
    |> createChaneSkinMarkup
    |> sendMessageMarkup context chatId messageId "Доступные скины для цитат:"

let proccessCommand context (command: CommandUpdate) =
    let chatId, messageId, command = command

    match command with
    | Start ->
        mainCommandsDescription context.Me.Username SingleChat
        |> singleStartMessage
        |> sendMessage context chatId messageId
    | SendChangeSkin -> sendChangeSkinMessage context chatId messageId

let resolveMessage message =
    match message with
    | Some(InaccessibleMessage _) -> sendError "Не могу получить сообщение: нет доступа"
    | Some(MaybeInaccessibleMessage.Message message) -> Ok message
    | None -> sendError "Не могу получить сообщение: нет сообщения"

let getQueryData (query: TCallbackQuery) =
    match query.Data with
    | Some data -> Ok data
    | None -> logError "Empty query"

let proccessQuery repository context (query: TCallbackQuery) =
    result {
        let! data = getQueryData query
        let! message = resolveMessage query.Message
        let chatId = message.Chat.Id
        let replyId = message.MessageId
        let parsed = split "," data

        match parsed with
        | query :: skinName :: _ when query = SET_SKIN ->
            let! skinInfo = skinByName skinName |> sendErrorIfNone "Не смог сменить скин. Неизвестное имя"
            do! repository.add chatId (Some skinName)
            let newSkinName = to_ru skinInfo.localization
            sendMessage context chatId replyId $"Смена скина произошла успешно. Новый скин для чата - {newSkinName}"
            return ()
        | _ -> return! logError "Unsupported query data"
    }

type AddToChatUpdate =
    { chatId: int64
      chatName: string
      botName: string }

let proccessAddToChat (original: UpdateContext) (update: AddToChatUpdate) =
    mainCommandsDescription update.botName GroupChat
    |> groupStartMessage update.botName
    |> sendMessage original update.chatId 0
