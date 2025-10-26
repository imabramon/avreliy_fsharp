module SimpleSkins

open System
open System.IO
open SixLabors.Fonts
open SixLabors.ImageSharp
open Image
open Skin
open Utils

let currentDir = Environment.CurrentDirectory

let fontPath =
    Path.Combine(currentDir, "./assets/MontserratAlternates-ExtraBold.ttf")

type Rect = { size: float32 pair; origin: Origin }

type TextInfo = { rect: Rect; color: Color option }

type AuthorInfo =
    { authorName: string
      getOrigin: Rect -> Origin }

type SimpleSkinInfo =
    { textInfo: TextInfo
      authorInfo: AuthorInfo
      backgroundPath: string }

let MIN_FONT_SIZE = 2f
let MAX_FONT_SIZE = 32f

let simpleSkin skinInfo text =
    result {
        let color = skinInfo.textInfo.color
        let rect = skinInfo.textInfo.rect
        let authorName = skinInfo.authorInfo.authorName
        let backgroundPath = skinInfo.backgroundPath

        let! fontFamily = getFontFamily fontPath
        let fontStyle = FontStyle()

        let quoteText =
            { value = text
              fontFamily = fontFamily
              style = fontStyle
              color = color }

        let quoteOrigin = rect.origin
        let quoteRect = rect.size
        let quoteSizeRange = MIN_FONT_SIZE, MAX_FONT_SIZE
        let quoteDraw = drawTextInRect quoteRect quoteSizeRange quoteText
        let resolvedRect = { rect with size = quoteDraw.size }


        let authorText =
            { value = authorName
              fontFamily = fontFamily
              style = fontStyle
              color = color }

        let authorOrigin = skinInfo.authorInfo.getOrigin resolvedRect
        let authorDraw = drawText MAX_FONT_SIZE authorText

        return
            { background = backgroundPath
              draw = [| quoteDraw.draw quoteOrigin; authorDraw.draw authorOrigin |] }
    }

let avrelii =
    let offsetX = 84f
    let offsetY = 20f

    let getOrigin quoteRect =
        let _, quoteHeight = quoteRect.size
        let quote = quoteRect.origin.origin

        { origin = PointF(quote.X + offsetX, quote.Y + quoteHeight / 2f + offsetY)
          position = Raw }

    let info =
        { backgroundPath = Path.Combine(currentDir, "./assets/avrelii.png")
          textInfo =
            { rect =
                { size = 680f, 480f
                  origin =
                    { origin = PointF(859f, 360f)
                      position = Centred } }
              color = Some Color.Black }
          authorInfo =
            { authorName = "Марк Аврелий"
              getOrigin = getOrigin } }

    simpleSkin info

let stetham =
    let offsetX = -96f
    let offsetY = 20f

    let getOrigin quoteRect =
        let _, quoteHeight = quoteRect.size
        let quote = quoteRect.origin.origin

        { origin = PointF(quote.X + offsetX, quote.Y + quoteHeight / 2f + offsetY)
          position = Raw }

    let info =
        { backgroundPath = Path.Combine(currentDir, "./assets/stetham.png")
          textInfo =
            { rect =
                { size = 510f, 565f
                  origin =
                    { origin = PointF(296f, 282f)
                      position = Centred } }
              color = Some Color.Black }
          authorInfo =
            { authorName = "Джейсон Стетхем"
              getOrigin = getOrigin } }

    simpleSkin info

let chad =
    let getOrigin _ =
        { origin = PointF(0f, 0f)
          position = Raw }

    let info =
        { backgroundPath = Path.Combine(currentDir, "./assets/chad.png")
          textInfo =
            { rect =
                { size = 770f, 565f
                  origin =
                    { origin = PointF(426f, 352f)
                      position = Centred } }
              color = Some Color.Black }
          authorInfo =
            { authorName = ""
              getOrigin = getOrigin } }

    simpleSkin info

let joker =
    let offsetX = 184f
    let offsetY = 20f

    let getOrigin quoteRect =
        let _, quoteHeight = quoteRect.size
        let quote = quoteRect.origin.origin

        { origin = PointF(quote.X + offsetX, quote.Y + quoteHeight / 2f + offsetY)
          position = Raw }

    let info =
        { backgroundPath = Path.Combine(currentDir, "./assets/joker.png")
          textInfo =
            { rect =
                { size = 680f, 515f
                  origin =
                    { origin = PointF(890f, 360f)
                      position = Centred } }
              color = Some Color.White }
          authorInfo =
            { authorName = "Джокер"
              getOrigin = getOrigin } }

    simpleSkin info
