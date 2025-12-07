module Program

open System
open DotNetEnv
open Funogram.Telegram.Bot

open Domain
open Utils
open Result
open Routing
open Database
open Errors

Env.Load() |> ignore

let initBot () =
    result {
        let! token = getToken
        let! db = initDb ()
        printfn $"Start bot init"

        let context =
            { repository = db
              validation = { startDate = DateTime.UtcNow } }

        startBot
            { Config.defaultConfig with
                Token = token }
            (update context)
            None
        |> Async.RunSynchronously
        |> ignore
    }


[<EntryPoint>]
let main _ =
    match initBot () with
    | Error e ->
        match e with
        | PublicError e -> printfn $"Can't send public error: {e.message}"
        | PrivateError e -> printfn $"System error: ${e}"

        -1
    | Ok _ -> 0
