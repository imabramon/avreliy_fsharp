module Image

open System
open System.IO
open System.Net.Http
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Drawing.Processing
open SixLabors.ImageSharp.Processing

open Utils
open Errors

type OriginPosition =
    | Centred
    | Raw

type Origin =
    { origin: PointF
      position: OriginPosition }

type TextStyle =
    { fontFamily: FontFamily
      style: FontStyle
      color: Color option }

type Ctx = IImageProcessingContext
type AbstactDrawJob = Ctx -> unit
type DrawJob = Origin -> AbstactDrawJob

type Draw =
    { size: float32 * float32
      draw: DrawJob }

let measureText (font: Font) (text: string) =
    let options = TextOptions(font)
    TextMeasurer.MeasureAdvance(text, options)

let wrapText (font: Font) maxWidth (text: string) =

    let words =
        text.Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList

    let getTextWidth text = (measureText font text).Width
    let spaceWidth = getTextWidth " "

    let wordWidths = words |> List.map (fun word -> word, getTextWidth word)

    let rec wrap text line lineWidth wordWidths =

        let textWithBreak = Helper.nonEmptyWith (text, "\n")
        let lineWithSpace = Helper.nonEmptyWith (line, " ")
        let widthWithSpace = Helper.nonEmptyWith (lineWidth, spaceWidth)

        match wordWidths with
        | [] -> textWithBreak + line
        | firstWord :: rest ->
            let word, width = firstWord
            let newWidth = widthWithSpace + width

            match newWidth with
            | _ when newWidth < maxWidth ->
                let newLine = lineWithSpace + word
                wrap text newLine newWidth rest
            | _ when newWidth = maxWidth ->
                let newText = textWithBreak + lineWithSpace + word + "\n"
                wrap newText "" 0f rest
            | _ ->
                let newText = textWithBreak + lineWithSpace
                wrap newText word width rest

    wrap "" "" 0f wordWidths


let rec binarySearch fn max low high =
    let mid = (low + high) / 2.0f
    let value = fn mid

    if high - low <= 0.1f then
        if value <= max then mid else low
    else
        match value <= max with
        | true -> binarySearch fn max mid high
        | false -> binarySearch fn max low mid

let findOptimalFontSize (fontFamily: FontFamily) (fontStyle: FontStyle) text maxWidth maxHeight minSize maxSize =

    let getHeight size =
        let currentFont = Font(fontFamily, size, fontStyle)
        let wrappedText = wrapText currentFont maxWidth text
        let wrapped = measureText currentFont wrappedText
        wrapped.Height

    let height = getHeight maxSize

    match height with
    | _ when height <= maxHeight -> maxSize
    | _ -> binarySearch getHeight maxHeight minSize maxSize

let isUrl (text: string) =
    text.StartsWith("http://") || text.StartsWith("https://")

let getImage (imagePath: string) =
    try
        match isUrl imagePath with
        | false ->
            let image = Image.Load(imagePath)
            Ok image
        | true ->
            use httpClient = new HttpClient()

            use stream = httpClient.GetStreamAsync(imagePath) |> Sync.run

            Ok(Image.Load stream)
    with e ->
        logError e.Message

let getFontFamily (fontPath: string) =
    let fontCollection = FontCollection()

    try
        let fontFamily = fontCollection.Add(fontPath)
        Ok fontFamily
    with e ->
        logError e.Message

let pointOf (origin: Origin) (rect: float32 * float32) =
    let x, y = origin.origin.X, origin.origin.Y
    let width, height = rect

    match origin.position with
    | Centred ->
        let x0 = x - (width / 2f)
        let y0 = y - (height / 2f)
        x0, y0
    | Raw -> x, y

let measureTextSize font text =
    let size = measureText font text
    size.Width, size.Height

let drawText (size: float32) (style: TextStyle) text =
    let font = Font(style.fontFamily, size, style.style)
    let size = measureTextSize font text
    let options = RichTextOptions(font)
    let color = style.color |> withDefault Color.Black

    let draw (origin: Origin) (ctx: IImageProcessingContext) =
        let x, y = pointOf origin size
        options.Origin <- PointF(x, y)
        ctx.DrawText(options, text, color) |> ignore

    { size = size; draw = draw }

let drawTextInRect (rect: float32 pair) (sizeRange: float32 pair) (style: TextStyle) text =
    let w, h = rect
    let min, max = sizeRange
    let fontSize = findOptimalFontSize style.fontFamily style.style text w h min max
    let font = Font(style.fontFamily, fontSize, style.style)
    let wrappedText = wrapText font w text
    let size = measureTextSize font wrappedText
    let color = style.color |> withDefault Color.Black

    let draw origin (ctx: Ctx) =
        let x, y = pointOf origin size
        let options = RichTextOptions(font)
        options.Origin <- PointF(x, y)
        ctx.DrawText(options, wrappedText, color) |> ignore

    { size = size; draw = draw }


let generateImage (image: Image) (outputPath: string) (jobs: AbstactDrawJob array) =
    Array.ForEach(jobs, (fun job -> image.Mutate job))
    image.Save outputPath

let centredIn x y =
    { origin = PointF(x, y)
      position = Centred }

let drawImage (image: Image) =
    let size = image.Width |> float32, image.Height |> float32

    let draw origin (ctx: Ctx) =
        let x, y = pointOf origin size
        ctx.DrawImage(image, Point(int x, int y), 1f) |> ignore

    { size = size; draw = draw }

let resizeImage (width: float32) (height: float32) (image: Image) =
    let resizeOptions = ResizeOptions()
    resizeOptions.Size <- Size(int width, int height)
    resizeOptions.Mode <- ResizeMode.Max

    image.Clone(fun ctx -> ctx.Resize(resizeOptions) |> ignore)

let applyCircleMask (image: Image) =
    let width = image.Width
    let height = image.Height
    let radius = float32 (min width height) / 2f
    let centerX = float32 width / 2f
    let centerY = float32 height / 2f

    let maskedImage = new Image<Rgba32>(width, height)

    maskedImage.Mutate(fun ctx ->
        ctx.Clip(
            EllipsePolygon(centerX, centerY, radius),
            fun clippedCtx -> clippedCtx.DrawImage(image, Point(0, 0), 1f) |> ignore
        )
        |> ignore)

    maskedImage
