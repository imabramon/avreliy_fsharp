module Result

open Errors

type ResultBilder<'TError>(zeroError: 'TError) =
    member _.Zero() = Error zeroError

    member _.Bind(x, f) =
        match x with
        | Ok x -> f x
        | Error e -> Error e

    member _.Return x = Ok x
    member _.ReturnFrom x = x

    member _.Using(disposable: #System.IDisposable, f) =
        try
            f disposable
        finally
            disposable.Dispose()

    member _.TryWith(tryBlock, catchHandler) =
        try
            tryBlock ()
        with e ->
            catchHandler e

    member _.TryFinally(tryBlock, finallyBlock) =
        try
            tryBlock ()
        finally
            finallyBlock ()

    member _.Delay f = f
    member _.Run f = f ()

    member _.Combine(a, с) =
        match a with
        | Ok _ -> с ()
        | Error e -> Error e

    member this.While(guard, body) =
        if not (guard ()) then
            Ok()
        else
            match body () with
            | Ok _ -> this.While(guard, body)
            | Error e -> Error e

    member _.For(sequence: seq<'T>, body: 'T -> Result<_, _>) =
        use enumerator = sequence.GetEnumerator()

        let rec loop () =
            if enumerator.MoveNext() then
                match body enumerator.Current with
                | Ok _ -> loop ()
                | Error e -> Error e
            else
                Ok()

        loop ()

let result = ResultBilder(PrivateError { message = "Nothing was returned" })

let ignoreResult x =
    match x with
    | Ok _ -> Ok()
    | Error e -> Error e

let toResult fn : Result<'a, ErrorExternal> =
    try
        Ok(fn ())
    with e ->
        logError e.Message

let resultAny (error: 'b) (list: Result<'a, 'b> list) =
    let results =
        list
        |> List.collect (fun res ->
            match res with
            | Ok o -> [ o ]
            | Error _ -> [])

    match results.Length with
    | x when x > 0 -> Ok results
    | _ -> Error error
