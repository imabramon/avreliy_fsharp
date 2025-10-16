module Handlers

open System
open System.IO
open Funogram.Api
open Funogram.Telegram.Bot
open Funogram.Types
open Skin
open Avrelii

// alias
type TelegramUpdate = Funogram.Telegram.Types.Update
type TelegramMessage = Funogram.Telegram.Types.Message

let processResultWithValue ifOk (result: Async<Result<'a, ApiResponseError>>) =
  async {
    let! result = result
    match result with
    | Ok _ -> ifOk()
    | Error e -> printfn "Server error: %s" e.Description
    
    return result
  }

let currentDir = __SOURCE_DIRECTORY__

let getImagePath () = 
    let tempFile = DateTime.Now.ToFileTimeUtc().ToString() + ".png"
    Path.Combine(currentDir, "./temp/" + tempFile)

let fontPath = Path.Combine(currentDir, "./assets/MontserratAlternates-ExtraBold.ttf")
let imageBackground = Path.Combine(currentDir,"./assets/avrelii.png")

let getInputFile path =
    let fileStream = new FileStream(path, FileMode.Open, FileAccess.Read)
    Funogram.Telegram.Types.InputFile.File(path, fileStream)

let dispose (f: Funogram.Telegram.Types.InputFile ) = 
    match f with
    | Funogram.Telegram.Types.InputFile.File (p, f) -> 
        match File.Exists(p) with
        | true -> File.Delete(p)
        | _ -> () 
        f.Dispose()
    | _ -> ()

let sendMessage context chatId text =
    let imagePath = getImagePath()
    match 
        text
        |> generateQuote imagePath avrelii
    with
    | Error e -> printfn $"{e}"
    | Ok _ ->
        let inputFile = getInputFile imagePath
        Funogram.Telegram.Api.sendPhoto chatId inputFile ""
        |> api context.Config
        |> processResultWithValue (fun _ -> 
            dispose inputFile
        )
        |> Async.Ignore
        |> Async.Start

// logic
let (|HasMessage|HasNoMessage|) (update: TelegramUpdate) =
    match update.Message with
    | Some message -> HasMessage message
    | None -> HasNoMessage

let onMessage context (message: TelegramMessage) =
    let chatId = message.Chat.Id

    match message.Text with
    | Some text -> text |> sendMessage context chatId
    | None -> printfn $"Some message without text"

let update context =
    context.Update.UpdateId |> printfn "Received update: %i"

    match context.Update with
    | HasMessage message -> onMessage context message
    | _ -> printfn "Recived nonmatchabe update"
