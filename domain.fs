module Domain

type PictureSkin = { path: string }

type Skin =
    | PictureSkin of PictureSkin
    | Sendler

type ChatContex = { id: int; skin: Skin }
