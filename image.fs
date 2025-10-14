module Image

open System
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.Drawing.Processing
open SixLabors.ImageSharp.Processing
open Utils

let measureText (font: Font) (text: string) =
        let options = TextOptions(font)
        TextMeasurer.MeasureAdvance(text, options)

let wrapText 
    (font:Font) 
    maxWidth 
    (text:string) = 

    let words = text.Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.toList
    let getTextWidth text = (measureText font text).Width
    let spaceWidth = getTextWidth " "

    let wordWidths = 
        words 
        |> List.map (fun word -> word, getTextWidth word)

    let rec wrap 
        text
        line 
        lineWidth 
        wordWidths =

        let textWithBreak = Helper.nonEmptyWith(text, "\n")
        let lineWithSpace = Helper.nonEmptyWith(line, " ")
        let widthWithSpace = Helper.nonEmptyWith(lineWidth, spaceWidth)
        
        match wordWidths with
        | [] -> textWithBreak + line
        | firstWord::rest ->
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
                let newText = textWithBreak + lineWithSpace;
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

let findOptimalFontSize 
    (fontFamily: FontFamily) 
    (fontStyle: FontStyle)
    text 
    maxWidth
    maxHeight
    minSize 
    maxSize =
    
    let getHeight size  =
        let currentFont = Font(fontFamily, size, fontStyle)
        let wrappedText = wrapText currentFont maxWidth text
        let wrapped = measureText currentFont wrappedText
        wrapped.Height

    let height = getHeight maxSize
    match height with
    | _ when height <= maxHeight ->
        maxSize
    | _ ->
        binarySearch getHeight maxHeight minSize maxSize

let getImage (imagePath:string) =
    try
        let image = Image.Load(imagePath)
        Ok image
    with
        e -> Error e.Message
let getFontFamily (fontPath: string) =
    let fontCollection = FontCollection()
    try 
        let fontFamily = fontCollection.Add(fontPath)
        Ok fontFamily
    with
        e -> Error e.Message 

let MAX_WIDTH = 680f
let MAX_HEIGHT = 480f
let MIN_FONT_SIZE = 2f
let MAX_FONT_SIZE = 32f
let AUTHOR_OFFSET_X = 84f
let AUTHOR_OFFSET_Y = 20f
let AUTHOR_NAME = "Марк Аврелий"

type TextPosition = 
    | Centred

let getRenderPositions 
    (mode: TextPosition) 
    x y 
    (rect: FontRectangle) 
    =
    match mode with
    | Centred ->
        let x0 = x - (rect.Width / 2f)
        let y0 = y - (rect.Height / 2f)
        x0, y0
let generateImage 
    imagePath
    fontPath
    outputPath
    xC
    yC
    (mode: TextPosition)
    text
    = result {
        use! image = getImage imagePath 
        let! fontFamily = getFontFamily fontPath
        let fontStyle = FontStyle()
        let fontSize = 
            findOptimalFontSize 
                fontFamily 
                fontStyle 
                text 
                MAX_WIDTH 
                MAX_HEIGHT 
                MIN_FONT_SIZE 
                MAX_FONT_SIZE
        let font = Font(fontFamily, fontSize, fontStyle)
        let wrappedText = wrapText font MAX_WIDTH text
        let rect = measureText font wrappedText
        let (x0, y0) = getRenderPositions mode xC yC rect
        let drawQuoteText  (ctx: IImageProcessingContext) =
            let options = RichTextOptions(font)
            options.Origin <- PointF(x0, y0)
            ctx.DrawText(options, wrappedText, Color.Black)
            |> ignore
        let drawAuthorText (ctx: IImageProcessingContext) =
            let authorFont = Font(fontFamily, 32f, fontStyle)
            let options = RichTextOptions(font)
            let width, heigh = rect.Width, rect.Height
            let x0, y0 = xC + AUTHOR_OFFSET_X, yC + heigh / 2f + AUTHOR_OFFSET_Y
            options.Origin <- PointF(x0, y0)
            ctx.DrawText(options, AUTHOR_NAME, Color.Black)
            |> ignore
        image.Mutate(drawQuoteText)
        image.Mutate(drawAuthorText)
        image.Save(outputPath)
        return 0
    }