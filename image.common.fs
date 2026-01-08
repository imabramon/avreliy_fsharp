module Image.Common

open System
open System.IO
open System.Net.Http
open SixLabors.Fonts
open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Drawing.Processing
open SixLabors.ImageSharp.Processing

open Utils.String
open Utils.Polymer
open Errors

type OriginPosition =
    | Centred
    | Raw

type Origin =
    { origin: PointF
      position: OriginPosition }

type Ctx = IImageProcessingContext
type AbstactDrawJob = Ctx -> unit
type DrawJob = Origin -> AbstactDrawJob

type Blueprint =
    { bounds: float32 * float32
      draw: DrawJob }

let getImage (imagePath: string) =
    try
        match isUrl imagePath with
        | false ->
            let image = Image.Load(imagePath)
            Ok image
        | true ->
            use httpClient = new HttpClient()

            use stream = httpClient.GetStreamAsync(imagePath) |> Polymer.sync

            Ok(Image.Load stream)
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


let generateImage (image: Image) (outputPath: string) (jobs: AbstactDrawJob array) =
    Array.ForEach(jobs, (fun job -> image.Mutate job))
    image.Save outputPath

let centredIn x y =
    { origin = PointF(x, y)
      position = Centred }
