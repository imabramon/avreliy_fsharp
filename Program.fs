module Program

open System
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

        startBot
            { Config.defaultConfig with
                Token = token }
            (update db)
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
