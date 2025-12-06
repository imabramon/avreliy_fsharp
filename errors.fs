module Errors

type ErrorInternal = { message: string }

type ErrorExternal =
    | PublicError of ErrorInternal
    | PrivateError of ErrorInternal

let err text = { message = text }
let privateError text = PrivateError(err text)
let publicError text = PublicError(err text)

let logError text = Error(privateError text)
let sendError text = Error(publicError text)

let getMessage e =
    match e with
    | PrivateError e -> e.message
    | PublicError e -> e.message

