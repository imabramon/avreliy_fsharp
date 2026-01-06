module Utils.String

open System

let split (separator: string) (str: string) =
    str.Split([| separator |], StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

let join separator strs =
    strs |> List.toArray |> String.concat separator

let toWords str = split " " str

let isUrl (text: string) =
    text.StartsWith("http://") || text.StartsWith("https://")
