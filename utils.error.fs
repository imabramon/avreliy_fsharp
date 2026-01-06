module Utils.Error

open Funogram.Types

open Errors

let errorIfNone ifNoneError x =
    match x with
    | Some x -> Ok x
    | None -> ifNoneError

let sendErrorIfNone text = errorIfNone (sendError text)

let logIfError (result: Async<Result<'a, ApiResponseError>>) =
    async {
        let! result = result

        match result with
        | Ok _ -> ignore ()
        | Error e -> printfn "Server error: %s" e.Description

        return result
    }
