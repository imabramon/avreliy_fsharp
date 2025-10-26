module Image

open System
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.Drawing.Processing
open SixLabors.ImageSharp.Processing
open Utils

type OriginPosition =
    | Centred
    | Raw

type Origin =
    { origin: PointF
      position: OriginPosition }

type Text =
    { value: string
      fontFamily: FontFamily
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

let getImage (imagePath: string) =
    try
        let image = Image.Load(imagePath)
        Ok image
    with e ->
        Error e.Message

let getFontFamily (fontPath: string) =
    let fontCollection = FontCollection()

    try
        let fontFamily = fontCollection.Add(fontPath)
        Ok fontFamily
    with e ->
        Error e.Message

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

let drawText (size: float32) (text: Text) =
    let font = Font(text.fontFamily, size, text.style)
    let size = measureTextSize font text.value
    let options = RichTextOptions(font)
    let color = text.color |> withDefault Color.Black

    let draw (origin: Origin) (ctx: IImageProcessingContext) =
        let x, y = pointOf origin size
        options.Origin <- PointF(x, y)
        ctx.DrawText(options, text.value, color) |> ignore

    { size = size; draw = draw }

let drawTextInRect (rect: float32 pair) (sizeRange: float32 pair) (text: Text) =
    let w, h = rect
    let min, max = sizeRange
    let fontSize = findOptimalFontSize text.fontFamily text.style text.value w h min max
    let font = Font(text.fontFamily, fontSize, text.style)
    let wrappedText = wrapText font w text.value
    let size = measureTextSize font wrappedText
    let color = text.color |> withDefault Color.Black

    let draw origin (ctx: Ctx) =
        let x, y = pointOf origin size
        let options = RichTextOptions(font)
        options.Origin <- PointF(x, y)
        ctx.DrawText(options, wrappedText, color) |> ignore

    { size = size; draw = draw }


let generateImage (image: Image) (outputPath: string) (jobs: AbstactDrawJob array) =
    Array.ForEach(jobs, (fun job -> image.Mutate job))
    image.Save outputPath
