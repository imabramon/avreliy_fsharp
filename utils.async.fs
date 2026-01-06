module Utils.Async

open Errors
open Utils.Error

let asyncStart req =
    req |> logIfError |> Async.Ignore |> Async.Start
