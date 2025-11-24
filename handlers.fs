module Handlers

open System
open System.IO
open Funogram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types

open Domain
open Skin
open Utils
open Maybe
open SimpleSkins
open Result
open Errors

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

type Command =
    | Start
    | SendChangeSkin

type CommandUpdate = Id * Id * Command

let sendMessageMarkup context chatId replyId message markup =
    let req = Funogram.Telegram.Api.sendMessageMarkup chatId message markup

    { req with
        ReplyParameters = Some(ReplyParameters.Create(replyId, Int chatId)) }
    |> api context.Config
    |> asyncStart

let SET_SKIN = "setSkin"

let createChaneSkinMarkup skinsInfo =
    skinsInfo
    |> Array.map (fun skinInfo ->
        [| InlineKeyboardButton.Create(skinInfo.publicName, callbackData = $"{SET_SKIN},{skinInfo.dbValue}") |])
    |> TInlineKeyboardMarkup.Create
    |> InlineKeyboardMarkup

let sendChangeSkinMessage context chatId messageId =
    availableSkins
    |> createChaneSkinMarkup
    |> sendMessageMarkup context chatId messageId "Доступные скины для цитат:"

let proccessCommand context (command: CommandUpdate) =
    let chatId, messageId, command = command

    match command with
    | Start -> sendMessage context chatId messageId "Привет я бот цитатник"
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
            do! repository.add chatId (Some skinName)
            printfn $"Change skin to {skinName} success"
            sendMessage context chatId replyId "Смена скина произошла успешно"
        | _ -> return! logError "Unsupported query data"
    }
