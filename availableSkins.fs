module AvailableSkins

open Skin
open SimpleSkins
open Localization
open SkinHelpers

let avreliiInfo =
    { name = "averlii"
      alias = [ "averlii"; "avr"; "Аврелий"; "аврелий"; "авр" ]
      skin = avrelii
      localization = ru "Аврелий" }

let stethamInfo =
    { name = "stetham"
      alias = [ "stetham"; "Стетхем"; "стетхем"; "стет" ]
      skin = stetham
      localization = ru "Стетхем" }

let chadInfo =
    { name = "chad"
      alias = [ "chad"; "чад"; "Chad"; "Чад" ]
      skin = chad
      localization = ru "Чад" }

let jokerInfo =
    { name = "joker"
      alias = [ "joker"; "joke"; "Joker"; "Джокер"; "джокер" ]
      skin = joker
      localization = ru "Джокер" }

let pepeWords = [ "Шнейне"; "Фа"; "Втфа"; "Пепе" ]

let pepeInfo =
    { name = "pepe"
      alias = [ "pepe"; "пепе"; "ганвест"; "Ганвест" ]
      skin = addRandomWords 3 pepeWords pepe
      localization = ru "Ганвест" }

let availabelSkins = [ avreliiInfo; chadInfo; stethamInfo; jokerInfo; pepeInfo ]

let infoByAlias =
    availabelSkins
    |> List.collect (fun info -> info.alias |> List.map (fun alias -> alias, info))
    |> Map

let infoByName = availabelSkins |> List.map (fun info -> info.name, info) |> Map

let skinByAlias alias =
    match infoByAlias.TryFind alias with
    | Some info -> Some info.skin
    | None -> None

let skinByName name = infoByName.TryFind name
