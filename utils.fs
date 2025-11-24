module Utils

open System
open DotNetEnv
open Funogram.Types

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

let errorIfNone ifNoneError x =
    match x with
    | Some x -> Ok x
    | None -> Error ifNoneError

let noneIfError x =
    match x with
    | Ok x -> x
    | Error e ->
        printfn $"Error: {e}"
        None

let ignoreResult x =
    match x with
    | Ok _ -> Ok()
    | Error e -> Error e

let withDefault defualtValue value =
    match value with
    | Some value -> value
    | None -> defualtValue

let ignoreMaybe x =
    match x with
    | Some _ -> ()
    | None -> ()

type Helper() =
    static member isZero(value: string) = value = ""
    static member isZero(value: int) = value = 0
    static member isZero(value: float32) = value = 0f

    static member nonEmptyWith(value: string, add: string) =
        if Helper.isZero (value) then value else value + add

    static member nonEmptyWith(value: int, add: int) =
        if Helper.isZero (value) then value else value + add

    static member nonEmptyWith(value: float32, add: float32) =
        if Helper.isZero (value) then value else value + add

type pair<'T> = 'T * 'T

let append arr elem = Array.append arr [| elem |]

let toResult fn : Result<'a, ErrorExternal> =
    try
        Ok(fn ())
    with e ->
        logError e.Message

let split (separator: string) (str: string) =
    str.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let join separator strs =
    strs |> List.toArray |> String.concat separator

let toWords str = split " " str

let logIfError (result: Async<Result<'a, ApiResponseError>>) =
    async {
        let! result = result

        match result with
        | Ok _ -> ignore ()
        | Error e -> printfn "Server error: %s" e.Description

        return result
    }

let asyncStart req =
    req |> logIfError |> Async.Ignore |> Async.Start

let unwrapOptionResult emptyMessage x =
    match x with
    | Some(Ok x) -> Ok x
    | Some(Error e) -> Error e
    | None -> logError emptyMessage
