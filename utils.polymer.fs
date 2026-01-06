module Utils.Polymer

type Polymer() =
    static member isZero(value: string) = value = ""
    static member isZero(value: int) = value = 0
    static member isZero(value: float32) = value = 0f

    static member nonEmptyWith(value: string, add: string) =
        if Polymer.isZero (value) then value else value + add

    static member nonEmptyWith(value: int, add: int) =
        if Polymer.isZero (value) then value else value + add

    static member nonEmptyWith(value: float32, add: float32) =
        if Polymer.isZero (value) then value else value + add

    static member sync(task: System.Threading.Tasks.Task<'a>) =
        task |> Async.AwaitTask |> Async.RunSynchronously

    static member sync(promise: Async<'a>) = promise |> Async.RunSynchronously
