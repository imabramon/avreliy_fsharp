module Utils

open System
open DotNetEnv

Env.Load() |> ignore


let getEnvVariable name =
    match Env.GetString(name) with
    | null -> Error $"Environment variable '{name}' is not defined"
    | value -> Ok value

let getToken =
    let mode = Environment.GetEnvironmentVariable("DEPLOY_MODE")

    match mode with
    | "prod" -> getEnvVariable "TOKEN"
    | "dev" -> getEnvVariable "TOKEN_DEV"
    | _ -> Error "Mode is not defined. Cant get Token"
