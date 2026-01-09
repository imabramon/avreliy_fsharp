module Image.Picture

open SixLabors.ImageSharp
open SixLabors.ImageSharp.Drawing
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Drawing.Processing
open SixLabors.ImageSharp.Processing

open Image.Common

let drawImage (image: Image) =
    let size = image.Width |> float32, image.Height |> float32

    let draw origin (ctx: Ctx) =
        let x, y = pointOf origin size
        ctx.DrawImage(image, Point(int x, int y), 1f) |> ignore

    { bounds = size; draw = draw }

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
