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
