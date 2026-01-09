module Skin

open System
open System.IO
open SixLabors.Fonts
open SixLabors.ImageSharp

open Image.Common
open Image.Text
open Image.Picture
open Result
open Utils.Common
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

type SkinContext =
    { authorName: string
      avatarPath: string
      textToQuote: string
      authorId: int64 }


let DEFAULT_AVATAR_PATH = "./assets/undefined.png"

let defaultSkinContext =
    { authorName = "Неизвестен"
      avatarPath = DEFAULT_AVATAR_PATH
      textToQuote = "Lorem ipsum и т.д"
      authorId = 0 }

let justText text =
    { defaultSkinContext with
        textToQuote = text }

type GenerateSkin = SkinContext -> Result<Skin, ErrorExternal>

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
        let authorDraw = getTextBlueprint MAX_FONT_SIZE style author.name
        append draws (authorDraw.draw origin)

let simpleSkin skinInfo context =
    result {
        let color = skinInfo.color
        let rect = skinInfo.quoteRect
        let backgroundPath = Path.Combine(currentDir, skinInfo.backgroundPath)

        let! fontFamily = getFontFamily fontPath
        let fontStyle = FontStyle()

        let style =
            { fontFamily = fontFamily
              style = fontStyle
              color = Some color
              options = None }

        let quoteOrigin = rect.origin
        let quoteRect = rect.size
        let quoteSizeRange = MIN_FONT_SIZE, MAX_FONT_SIZE

        let quoteDraw =
            getTextInRectBlueprint quoteRect quoteSizeRange style context.textToQuote

        let resolvedRect = { rect with size = quoteDraw.bounds }

        let baseDraws = [| quoteDraw.draw quoteOrigin |]

        return
            { background = backgroundPath
              draw = addAuthorDraw skinInfo.author resolvedRect style baseDraws }
    }

let selfSkin (context: SkinContext) =
    result {
        let color = Color.White

        let rect =
            { size = 680f, 513f
              origin = centredIn 905f 360f }

        let backgroundPath = Path.Combine(currentDir, "./assets/self.png")

        let! fontFamily = getFontFamily fontPath
        let fontStyle = FontStyle()
        let font = Font(fontFamily, MAX_FONT_SIZE)

        let options =
            { border = Some { width = 5f; color = Color.Black }
              shadow = Some { offset = 3f, 3f; color = Color.Black } }

        let style =
            { fontFamily = fontFamily
              style = fontStyle
              color = Some color
              options = Some options }

        let quoteOrigin = rect.origin
        let quoteRect = rect.size
        let quoteSizeRange = MIN_FONT_SIZE, MAX_FONT_SIZE

        let quoteDraw =
            getTextInRectBlueprint quoteRect quoteSizeRange style context.textToQuote

        let resolvedRect = { rect with size = quoteDraw.bounds }

        let! avatar =
            getImage context.avatarPath
            |> map (resizeImage 364f 364f)
            |> map applyCircleMask

        let avatarDraw = drawImage avatar

        let avatarOrigin =
            { position = Raw
              origin = PointF(100f, 178f) }

        let baseDraws = [| quoteDraw.draw quoteOrigin; avatarDraw.draw avatarOrigin |]

        let authorRect = measureText font context.authorName

        let authorOffset = 334f - authorRect.Width

        let authorInfo =
            Some
                { name = context.authorName
                  offset = authorOffset }

        return
            { background = backgroundPath
              draw = addAuthorDraw authorInfo resolvedRect style baseDraws }
    }
