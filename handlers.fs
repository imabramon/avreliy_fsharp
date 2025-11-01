module Handlers

open System
open System.IO
open Funogram.Api
open Funogram.Telegram.Bot

open Domain
open Skin
open Utils

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
            match File.Exists p with
            | true -> File.Delete p
            | _ -> ()

            f.Dispose()
        | _ -> ()

        return result
    }

type TextUpdate =
    { skin: string -> Result<Skin, string>
      chatId: Id
      text: string
      replyMessageId: Id }

let sendPhoto (update: TextUpdate) chatId inputFile =
    let sendler = Funogram.Telegram.Api.sendPhoto chatId inputFile ""
    let chatId = TChatId.Int chatId
    let replyInfo = TReplyParams.Create(update.replyMessageId, chatId)

    { sendler with
        ReplyParameters = Some replyInfo }

let sendQuote context (update: TextUpdate) =
    let imagePath = getImagePath ()
    let textUpdate = update

    match textUpdate.text |> generateQuote imagePath update.skin with
    | Error e -> Error e
    | Ok _ ->
        let inputFile = getInputFile imagePath

        sendPhoto update textUpdate.chatId inputFile
        |> api context.Config
        |> logIfError
        |> dispose inputFile
        |> Async.Ignore
        |> Async.Start

        Ok 0

let sendMessage context chatId message =
    Funogram.Telegram.Api.sendMessage chatId message
    |> api context.Config
    |> logIfError
    |> Async.Ignore
    |> Async.Start

type Command =
    | Start
    | SendChangeSkin
    | ChangeSkin of string

type CommandUpdate = Id * Command

let proccessCommand context (command: CommandUpdate) =
    let chatId, command = command

    match command with
    | Start -> sendMessage context chatId "Привет я бот цитатник"
    | _ -> printfn "Command is not implemented"

let proccessQuery (query: TCallbackQuery) = ignore
