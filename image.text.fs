module Image.Text

open System
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing.Processing
open SixLabors.ImageSharp.Processing

open Image.Common
open Utils.Common
open Utils.Polymer
open Maybe
open Errors

type Border = { width: float32; color: Color }

type Shadow = { offset: float32 pair; color: Color }

type TextStyleOptions =
    { border: Border option
      shadow: Shadow option }

type TextStyle =
    { fontFamily: FontFamily
      style: FontStyle
      color: Color option
      options: TextStyleOptions option }

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

        let textWithBreak = Polymer.nonEmptyWith (text, "\n")
        let lineWithSpace = Polymer.nonEmptyWith (line, " ")
        let widthWithSpace = Polymer.nonEmptyWith (lineWidth, spaceWidth)

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

let measureTextSize font text =
    let size = measureText font text
    size.Width, size.Height

let borderOf textStyle =
    match textStyle.options with
    | None -> None
    | Some options -> options.border

let shadowOf textStyle =
    match textStyle.options with
    | None -> None
    | Some options -> options.shadow

let drawText (ctx: IImageProcessingContext) (font: Font) (color: Color) (point: PointF) (text: string) =
    ctx.DrawText(text, font, Brushes.Solid color, point) |> ignore

let drawTextShadow (ctx: IImageProcessingContext) (font: Font) (point: PointF) (shadow: Shadow option) (text: string) =
    maybe {
        let! shadow = shadow
        let x, y = shadow.offset
        let point = PointF(point.X + x, point.Y + y)
        ctx.DrawText(text, font, Brushes.Solid shadow.color, point) |> ignore
        return ()
    }
    |> ignore


let getTextBlueprint (fontSize: float32) (style: TextStyle) text =
    let font = Font(style.fontFamily, fontSize, style.style)
    let size = measureTextSize font text
    let color = style.color |> withDefault Color.Black

    let draw (origin: Origin) (ctx: IImageProcessingContext) =
        let x, y = pointOf origin size
        let point = PointF(x, y)
        do drawTextShadow ctx font point (shadowOf style) text
        do drawText ctx font color point text

    { bounds = size; draw = draw }

let getTextInRectBlueprint (rect: float32 pair) (sizeRange: float32 pair) (style: TextStyle) text =
    let w, h = rect
    let min, max = sizeRange
    let fontSize = findOptimalFontSize style.fontFamily style.style text w h min max
    let font = Font(style.fontFamily, fontSize, style.style)
    let wrappedText = wrapText font w text
    let size = measureTextSize font wrappedText
    let color = style.color |> withDefault Color.Black

    let draw origin (ctx: Ctx) =
        let x, y = pointOf origin size
        let point = PointF(x, y)
        do drawTextShadow ctx font point (shadowOf style) wrappedText
        do drawText ctx font color point wrappedText

    { bounds = size; draw = draw }

let getFontFamily (fontPath: string) =
    let fontCollection = FontCollection()

    try
        let fontFamily = fontCollection.Add(fontPath)
        Ok fontFamily
    with e ->
        logError e.Message

let getRectOrigin (rect: Rect) =
    pointOf rect.origin rect.size

let addCaptions draws (captionsForRect: Rect) size style (gap: float32) (captions: string list) =
    let x0, y0 = getRectOrigin captionsForRect
    let w, h = captionsForRect.size
    let x1, y1 = x0, y0 + h + gap 

    let rec addCaption acc x y captions =
        match captions with
        | caption :: rest ->
            let captionBlueprint = getTextBlueprint size style caption
            let capW, capH = captionBlueprint.bounds
            let offsetX = w - capW
            let captionOrigin = {
                position = Raw
                origin = PointF(x + offsetX, y)
            }
            let acc = append acc (captionBlueprint.draw captionOrigin)
            let y = y + capH + gap
            addCaption acc x y rest
        | _ -> acc

    addCaption draws x1 y1 captions 
