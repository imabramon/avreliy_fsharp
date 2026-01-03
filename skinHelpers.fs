module SkinHelpers

open Utils
open Skin

let addRandomWords freq words (generate: GenerateSkin) context =
    let text =
        split " " context.textToQuote
        |> List.mapi (fun i w ->
            match i % freq with
            | 0 -> w + " " + pickRandom words
            | _ -> w)
        |> join " "

    let context = { context with textToQuote = text }
    generate context
