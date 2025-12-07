module Domain

open System
open Funogram.Telegram.Bot

type TUpdate = Funogram.Telegram.Types.Update
type TMessage = Funogram.Telegram.Types.Message
type TChatType = Funogram.Telegram.Types.ChatType
type TChat = Funogram.Telegram.Types.Chat
type TReplyParams = Funogram.Telegram.Types.ReplyParameters
type TChatId = Funogram.Telegram.Types.ChatId
type TCallbackQuery = Funogram.Telegram.Types.CallbackQuery
type TInlineKeyboardButton = Funogram.Telegram.Types.InlineKeyboardButton
type TInlineKeyboardMarkup = Funogram.Telegram.Types.InlineKeyboardMarkup
type TChatMember = Funogram.Telegram.Types.ChatMemberUpdated
type TChannelPost = Funogram.Telegram.Types.Message
type TPoll = Funogram.Telegram.Types.Poll
type Id = int64

type SupportedUpdates =
    | Message of TMessage
    | ChatMember of TChatMember
    | ChannelPost of TChannelPost
    | Poll of TPoll
    | CallbackQuery of TCallbackQuery
    | AddToChat of TChatMember
    | UnsupportedUpdate

let resolveSupportedUpdate (context: UpdateContext) : SupportedUpdates =
    match
        context.Update.Message,
        context.Update.ChatMember,
        context.Update.ChannelPost,
        context.Update.Poll,
        context.Update.CallbackQuery,
        context.Update.MyChatMember
    with
    | (Some m, _, _, _, _, _) -> Message m
    | (_, Some cm, _, _, _, _) -> ChatMember cm
    | (_, _, Some cp, _, _, _) -> ChannelPost cp
    | (_, _, _, Some p, _, _) -> Poll p
    | (_, _, _, _, Some cq, _) -> CallbackQuery cq
    | (_, _, _, _, _, Some mm) -> AddToChat mm
    | _ -> UnsupportedUpdate

let getTime update =
    match update with
    | Message m -> Some(m.Date.ToUniversalTime())
    | ChatMember cm -> Some(cm.Date.ToUniversalTime())
    | ChannelPost cp -> Some(cp.Date.ToUniversalTime())
    | AddToChat mm -> Some(mm.Date.ToUniversalTime())
    | _ -> None

type ChatRepository<'TError> =
    { add: int64 -> string option -> Result<unit, 'TError>
      get: int64 -> Result<string option, 'TError> }

type ValidationConfig = { startDate: DateTime }

type BotContext<'TError> =
    { repository: ChatRepository<'TError>
      validation: ValidationConfig }

type ResolvedChatType =
    | SingleChat
    | GroupChat
