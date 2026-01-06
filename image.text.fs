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

type TextStyleOptions = { border: Border option }

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

let drawTextBorder (ctx: IImageProcessingContext) (options: RichTextOptions) x y (text: string) style =
    maybe {
        let! border = borderOf style
        let color = border.color
        let drawing = new DrawingOptions()

        ctx.DrawText(drawing, options, text, Brushes.Solid(color), Pens.Solid(color, border.width))
        |> ignore

        return ()
    }
    |> ignore

let drawText (size: float32) (style: TextStyle) text =
    let font = Font(style.fontFamily, size, style.style)
    let size = measureTextSize font text
    let options = RichTextOptions(font)
    let color = style.color |> withDefault Color.Black

    let draw (origin: Origin) (ctx: IImageProcessingContext) =
        let x, y = pointOf origin size
        options.Origin <- PointF(x, y)
        do drawTextBorder ctx options x y text style
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
        let options = RichTextOptions font
        options.Origin <- PointF(x, y)
        do drawTextBorder ctx options x y wrappedText style
        ctx.DrawText(options, wrappedText, color) |> ignore

    { size = size; draw = draw }

let getFontFamily (fontPath: string) =
    let fontCollection = FontCollection()

    try
        let fontFamily = fontCollection.Add(fontPath)
        Ok fontFamily
    with e ->
        logError e.Message
