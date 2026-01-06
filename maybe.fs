module Maybe

type MaybeBuilder() =
    member _.Bind(opt, binder) =
        match opt with
        | Some value -> binder value
        | None -> None

    member _.Return(value) = Some value
    member _.ReturnFrom(opt) = opt
    member _.Zero() = None

let maybe = MaybeBuilder()

let withDefault defualtValue value =
    match value with
    | Some value -> value
    | None -> defualtValue

let noneIfError x =
    match x with
    | Ok x -> x
    | Error e ->
        printfn $"Error: {e}"
        None
