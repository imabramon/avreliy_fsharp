module Utils.Common

open System

// ===================Types======================

type pair<'T> = 'T * 'T

// =====================Seq======================

let append arr elem = Array.append arr [| elem |]

let pickRandom (list: 'a list) =
    list |> List.item (Random().Next list.Length)

// =====================Algo=====================

let rec binarySearch fn max low high =
    let mid = (low + high) / 2.0f
    let value = fn mid

    if high - low <= 0.1f then
        if value <= max then mid else low
    else
        match value <= max with
        | true -> binarySearch fn max mid high
        | false -> binarySearch fn max low mid
