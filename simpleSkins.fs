module SimpleSkins

open System
open System.IO
open SixLabors.Fonts
open SixLabors.ImageSharp

open Skin
open Image
open Utils
open Result

let currentDir = Environment.CurrentDirectory

let fontPath =
    Path.Combine(currentDir, "./assets/MontserratAlternates-ExtraBold.ttf")

type Rect = { size: float32 pair; origin: Origin }

type AuthorInfo = { name: string; offset: float32 }

type SimpleSkinInfoV2 =
    { backgroundPath: string
      quoteRect: Rect
      author: AuthorInfo option
      color: Color }

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

let avrelii =
    simpleSkin
        { backgroundPath = "./assets/avrelii.png"
          quoteRect =
            { origin = centredIn 859f 360f
              size = 680f, 480f }
          author = Some { name = "Марк Аврелий"; offset = 84f }
          color = Color.Black }

let stetham =
    simpleSkin
        { backgroundPath = "./assets/stetham.png"
          quoteRect =
            { origin = centredIn 296f 282f
              size = 510f, 565f }
          author =
            Some
                { name = "Джейсон Стетхем"
                  offset = -96f }
          color = Color.Black }

let chad =
    simpleSkin
        { backgroundPath = "./assets/chad.png"
          quoteRect =
            { origin = centredIn 426f 352f
              size = 770f, 565f }
          author = None
          color = Color.Black }

let joker =
    simpleSkin
        { backgroundPath = "./assets/joker.png"
          quoteRect =
            { origin = centredIn 890f 360f
              size = 680f, 515f }
          author = Some { name = "Джокер"; offset = 184f }
          color = Color.White }

let availableSkins =
    [| { publicName = "Марк Аврелий"
         dbValue = "avrelii" }
       { publicName = "Джейсон Стетхем"
         dbValue = "stetham" }
       { publicName = "Чад"; dbValue = "chad" }
       { publicName = "Джокер"
         dbValue = "joker" } |]
