module Handlers

open Funogram.Api
open Funogram.Telegram.Bot

// alias
type TelegramUpdate = Funogram.Telegram.Types.Update
type TelegramMessage = Funogram.Telegram.Types.Message

let sendMessage context chatId text =
    text
    |> Funogram.Telegram.Api.sendMessage chatId
    |> api context.Config
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
