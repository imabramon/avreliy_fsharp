module Skin

open System
open System.IO
open SixLabors.Fonts
open SixLabors.ImageSharp

open Image
open Result
open Utils
open Errors
open Localization

type Skin =
    { background: string
      draw: AbstactDrawJob array }

type Rect = { size: float32 pair; origin: Origin }

type AuthorInfo = { name: string; offset: float32 }

type SimpleSkinInfoV2 =
    { backgroundPath: string
      quoteRect: Rect
      author: AuthorInfo option
      color: Color }

type GenerateSkin = string -> Result<Skin, ErrorExternal>

type TSkinInfo =
    { name: string
      skin: GenerateSkin
      alias: string list
      localization: Localization }

let generateQuote path skin quote =
    result {
        let! skin = skin quote
        use! image = getImage skin.background
        skin.draw |> generateImage image path
        return ()
    }

let currentDir = Environment.CurrentDirectory

let fontPath =
    Path.Combine(currentDir, "./assets/MontserratAlternates-ExtraBold.ttf")

let MIN_FONT_SIZE = 2f
let MAX_FONT_SIZE = 32f

let getOrigin quoteRect offsetX =
    let offsetY = 20f
    let _, quoteHeight = quoteRect.size
    let quote = quoteRect.origin.origin

    { origin = PointF(quote.X + offsetX, quote.Y + quoteHeight / 2f + offsetY)
      position = Raw }

let addAuthorDraw author rect style draws =
    match author with
    | None -> draws
    | Some author ->
        let origin = getOrigin rect author.offset
        let authorDraw = drawText MAX_FONT_SIZE style author.name
        append draws (authorDraw.draw origin)

let simpleSkin skinInfo text =
    result {
        let color = skinInfo.color
        let rect = skinInfo.quoteRect
        let backgroundPath = Path.Combine(currentDir, skinInfo.backgroundPath)

        let! fontFamily = getFontFamily fontPath
        let fontStyle = FontStyle()

        let style =
            { fontFamily = fontFamily
              style = fontStyle
              color = Some color }

        let quoteOrigin = rect.origin
        let quoteRect = rect.size
        let quoteSizeRange = MIN_FONT_SIZE, MAX_FONT_SIZE
        let quoteDraw = drawTextInRect quoteRect quoteSizeRange style text
        let resolvedRect = { rect with size = quoteDraw.size }

        let baseDraws = [| quoteDraw.draw quoteOrigin |]

        return
            { background = backgroundPath
              draw = addAuthorDraw skinInfo.author resolvedRect style baseDraws }
    }
