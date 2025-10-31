module Skin

open Image
open Result

type Skin =
    { background: string
      draw: AbstactDrawJob array }

let generateQuote path skin quote =
    result {
        let! skin = skin quote
        use! image = getImage skin.background
        skin.draw |> generateImage image path
        return 0
    }
