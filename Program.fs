module Program

open System
open DotNetEnv
open Funogram.Telegram.Bot
open Utils
open Handlers

Env.Load() |> ignore

let initBot token =
    printfn $"Start bot init"

    startBot
        { Config.defaultConfig with
            Token = token }
        update
        None
    |> Async.RunSynchronously
    |> ignore

[<EntryPoint>]
let main _ =
    match getToken with
    | Error e -> printfn $"Error: {e}"
    | Ok token -> initBot token

    0
