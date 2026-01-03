module SkinHelpers

open Utils
open Skin

let addRandomWords freq words (generate: GenerateSkin) text =
    split " " text
    |> List.mapi (fun i w ->
        match i % freq with
        | 0 -> w + " " + pickRandom words
        | _ -> w)
    |> join " "
    |> generate
