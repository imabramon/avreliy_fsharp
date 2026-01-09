module Utils.IO

open System
open System.Globalization
open System.Threading
open DotNetEnv

open Errors

Env.Load() |> ignore

let getEnvVariable name =
    match Env.GetString(name) with
    | null -> logError $"Environment variable '{name}' is not defined"
    | value -> Ok value

let getToken =
    let mode = Environment.GetEnvironmentVariable "DEPLOY_MODE"

    match mode with
    | "prod" -> getEnvVariable "TOKEN"
    | "dev" -> getEnvVariable "TOKEN_DEV"
    | _ -> logError "Mode is not defined. Cant get Token"

let setRussianCulture () =
    let russianCulture = CultureInfo("ru-RU")

    Thread.CurrentThread.CurrentCulture <- russianCulture
    Thread.CurrentThread.CurrentUICulture <- russianCulture

let getRussianLongDate (date: DateTime) =
    setRussianCulture ()
    date.ToLongDateString()
