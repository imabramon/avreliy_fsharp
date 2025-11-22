module Program

open System
open Domain
open DotNetEnv
open Funogram.Telegram.Bot
open Utils
open Result
open Routing
open Database

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
        printfn $"Error: {e}"
        -1
    | Ok token -> 0
