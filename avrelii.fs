module Avrelii

open System.IO
open SixLabors.Fonts
open SixLabors.ImageSharp
open Image
open Skin
open Utils

let currentDir = __SOURCE_DIRECTORY__
let MAX_WIDTH = 680f
let MAX_HEIGHT = 480f
let MIN_FONT_SIZE = 2f
let MAX_FONT_SIZE = 32f
let QUOTE_X = 859f
let QUOTE_Y = 360f
let AUTHOR_OFFSET_X = 84f
let AUTHOR_OFFSET_Y = 20f
let AUTHOR_NAME = "Марк Аврелий"

let fontPath = Path.Combine(currentDir, "./assets/MontserratAlternates-ExtraBold.ttf")
let backgroundPath = Path.Combine(currentDir,"./assets/avrelii.png")


let avrelii text = 
    result {
        let! fontFamily = getFontFamily fontPath
        let fontStyle = FontStyle()

        let quoteText = {
            value = text
            fontFamily = fontFamily
            style = fontStyle
        }
        let quoteOrigin = {
            origin = PointF(QUOTE_X, QUOTE_Y)
            position = Centred
        }
        let quoteRect = MAX_WIDTH, MAX_HEIGHT
        let quoteSizeRange = MIN_FONT_SIZE, MAX_FONT_SIZE
        let quoteDraw = drawTextInRect quoteRect quoteSizeRange quoteText
        let _, quoteHeight = quoteDraw.size

        let authorText = {
            value = AUTHOR_NAME
            fontFamily = fontFamily
            style = fontStyle
        }
        let authorOrigin = {
            origin = PointF(
                QUOTE_X + AUTHOR_OFFSET_X, 
                QUOTE_Y + quoteHeight / 2f + AUTHOR_OFFSET_Y
            )
            position = Raw
        }
        let authorDraw = drawText MAX_FONT_SIZE authorText
        

        return {
            background = backgroundPath
            draw = [|
                quoteDraw.draw quoteOrigin
                authorDraw.draw authorOrigin
            |]
        }
    }