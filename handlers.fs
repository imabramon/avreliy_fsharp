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

let processResultWithValue ifOk (result: Async<Result<'a, ApiResponseError>>) =
  async {
    let! result = result
    match result with
    | Ok _ -> ifOk()
    | Error e -> printfn "Server error: %s" e.Description
    
    return result
  }

let getImagePath () = 
    let tempFile = DateTime.Now.ToFileTimeUtc().ToString() + ".png"
    Path.Combine(Environment.CurrentDirectory, "./temp/" + tempFile)

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
    | Error e -> Error e
    | Ok _ ->
        let inputFile = getInputFile imagePath
        Funogram.Telegram.Api.sendPhoto chatId inputFile ""
        |> api context.Config
        |> processResultWithValue (fun _ -> 
            dispose inputFile
        )
        |> Async.Ignore
        |> Async.Start
        Ok 0

let withNoTextError = errorIfNone "Message has no text"

// logic
let getTextForGroup (context: UpdateContext) (message: TelegramMessage) =
    result {
        let! selfText =
            message.Text |> withNoTextError
        let! replyMessage =
            message.ReplyToMessage |> errorIfNone "Has no reply message"
        let! botName = context.Me.Username |> errorIfNone "Cant get bot username"
        return!
            match selfText = $"@{botName}" with 
            | true -> replyMessage.Text |> errorIfNone "Has no reply text"
            | _ -> Error "Has no bot name when reply"
    }

let getChatId (context: UpdateContext) =
    match context.Update.Message with 
    | Some message -> Ok message.Chat.Id
    | None -> Error "Message hasnt chat id"
    
let getText  (context: UpdateContext) = 
    result {
        let! message = 
            context.Update.Message 
            |> errorIfNone "Update has no message"
        let chat = message.Chat
        let chatType = chat.Type
        return!
            match chatType with
            | TelegramChatType.Private -> 
                    message.Text
                    |> withNoTextError
            | TelegramChatType.SuperGroup
            | TelegramChatType.Group ->
                getTextForGroup context message
            | _ -> Error "No implemented chat type"
    }

let update context =
    printfn $"Get update: {context.Update.UpdateId}"
    match result {
        let! chatId = getChatId context
        let! text = getText context
        return! sendMessage context chatId text
    } with
    | Ok _ -> printfn "Update process ok" 
    | Error e -> printfn $"Error: {e}"
