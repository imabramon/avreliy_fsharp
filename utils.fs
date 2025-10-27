module Utils

open System
open DotNetEnv

Env.Load() |> ignore


let getEnvVariable name =
    match Env.GetString(name) with
    | null -> Error $"Environment variable '{name}' is not defined"
    | value -> Ok value

let getToken =
    let mode = Environment.GetEnvironmentVariable "DEPLOY_MODE"

    match mode with
    | "prod" -> getEnvVariable "TOKEN"
    | "dev" -> getEnvVariable "TOKEN_DEV"
    | _ -> Error "Mode is not defined. Cant get Token"

type ResultBilder() =
    member b.Zero() = Error "Nothing has returned"

    member b.Bind(x, f) =
        match x with
        | Ok x -> f x
        | Error e -> Error e

    member b.Return x = Ok x
    member b.ReturnFrom x = x

    member b.Using((disposable: #System.IDisposable), f) =
        try
            f disposable
        finally
            disposable.Dispose()

let result = ResultBilder()

let errorIfNone ifNoneError x =
    match x with
    | Some x -> Ok x
    | None -> Error ifNoneError

let withDefault defualtValue value =
    match value with
    | Some value -> value
    | None -> defualtValue

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

type MaybeBuilder() =
    member _.Bind(opt, binder) =
        match opt with
        | Some value -> binder value
        | None -> None

    member _.Return(value) = Some value
    member _.ReturnFrom(opt) = opt
    member _.Zero() = None

let maybe = MaybeBuilder()

let append arr elem = Array.append arr [| elem |]
