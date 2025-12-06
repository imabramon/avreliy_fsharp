module SimpleSkins

open SixLabors.ImageSharp

open Skin
open Image

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
