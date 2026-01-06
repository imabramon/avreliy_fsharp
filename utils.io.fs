module Utils.IO

open System
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
