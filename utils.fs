module Utils

open System
open DotNetEnv
open Funogram.Types

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

    member b.Using(disposable: #System.IDisposable, f) =
        try
            f disposable
        finally
            disposable.Dispose()

    member b.TryWith(tryBlock, catchHandler) =
        try
            tryBlock ()
        with e ->
            catchHandler e

    member b.TryFinally(tryBlock, finallyBlock) =
        try
            tryBlock ()
        finally
            finallyBlock ()

    member b.Delay(f) = f
    member b.Run(f) = f ()

    member b.Combine(a, с) =
        match a with
        | Ok _ -> с ()
        | Error e -> Error e

    member b.While(guard, body) =
        if not (guard ()) then
            Ok()
        else
            match body () with
            | Ok _ -> b.While(guard, body)
            | Error e -> Error e

    member b.For(sequence: seq<'T>, body: 'T -> Result<_, _>) =
        use enumerator = sequence.GetEnumerator()

        let rec loop () =
            if enumerator.MoveNext() then
                match body enumerator.Current with
                | Ok _ -> loop ()
                | Error e -> Error e
            else
                Ok()

        loop ()

let result = ResultBilder()

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

let toResult fn =
    try
        Ok(fn ())
    with e ->
        Error e.Message

let split (separator: string) (str: string) =
    str.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let toWords str = split " " str

let logIfError (result: Async<Result<'a, ApiResponseError>>) =
    async {
        let! result = result

        match result with
        | Ok _ -> ignore ()
        | Error e -> printfn "Server error: %s" e.Description

        return result
    }
