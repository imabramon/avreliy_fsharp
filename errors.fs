module Errors

type ErrorInternal = { message: string }

type ErrorExternal =
    | PublicError of ErrorInternal
    | PrivateError of ErrorInternal

let err text = { message = text }

let logError text = Result.Error(PrivateError(err text))
let sendError text = Result.Error(PublicError(err text))
