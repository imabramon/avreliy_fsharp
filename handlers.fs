module Handlers

open System
open System.IO
open Funogram.Api
open Funogram.Telegram.Bot
open Funogram.Telegram.Types

open Domain
open Skin
open Utils
open Result
open Errors
open Localization
open AvailableSkins
open Commands
open Text

let getImagePath () =
    let tempFile = DateTime.Now.ToFileTimeUtc().ToString() + ".png"
    Path.Combine(Environment.CurrentDirectory, "./temp/" + tempFile)

let getInputFile path =
    let fileStream = new FileStream(path, FileMode.Open, FileAccess.Read)
    InputFile.File(path, fileStream)

let getInputFileAsBytes path =
    let bytes = File.ReadAllBytes(path)
    let fileName = Path.GetFileName(path)
    FileBytes(fileName, bytes)

let dispose (f: Funogram.Telegram.Types.InputFile) =
    match f with
    | InputFile.File(p, f) ->
        match File.Exists p with
        | true -> File.Delete p
        | _ -> ()

        f.Dispose()
    | FileBytes(p, _) ->
        let p = Path.Combine(Environment.CurrentDirectory, "./temp/" + p)
        File.Delete p
    | _ -> ()

let disposeFile (f: Funogram.Telegram.Types.InputFile) result =
    async {
        let! res = result
        dispose f
        return res
    }

let disposeFileList (f: Funogram.Telegram.Types.InputFile list) result =
    async {
        let! res = result
        f |> List.iter dispose
        return res
    }

type TextUpdate =
    { skin: string -> Result<Skin, ErrorExternal>
      chatId: Id
      text: string
      replyMessageId: Id }

let sendPhoto (update: TextUpdate) chatId inputFile text =
    let sendler = Funogram.Telegram.Api.sendPhoto chatId inputFile ""
    let chatId = Int chatId
    let replyInfo = ReplyParameters.Create(update.replyMessageId, chatId)

    { sendler with
        ReplyParameters = Some replyInfo
        Caption = Some text }

let fileToMedia (inputFile: InputFile) =
    InputMedia.Photo(InputMediaPhoto.Create("photo", inputFile))

let sendMediaGroup chatId messageId inputFiles =
    let media = inputFiles |> List.map fileToMedia |> List.toArray
    let sendler = Funogram.Telegram.Api.sendMediaGroup chatId media
    let chatId = Int chatId
    let replyInfo = ReplyParameters.Create(messageId, chatId)

    { sendler with
        ReplyParameters = Some replyInfo }

let generateQuote inputFileGetter skin text =
    result {
        let imagePath = getImagePath ()

        do! generateQuote imagePath skin text

        return inputFileGetter imagePath
    }

let sendExamples context chatId messageId =
    result {
        let generateQuote = generateQuote getInputFileAsBytes
        let resultAny = resultAny (publicError "Не вышло сгененрировать ни одной цитаты")

        let! files =
            availabelSkins
            |> List.map (fun info ->
                let alias = join ", " info.alias
                generateQuote info.skin $"Доступные алиасы: {alias}")
            |> resultAny

        sendMediaGroup chatId messageId files
        |> api context.Config
        |> disposeFileList files
        |> asyncStart

        return ()
    }


let sendQuote context (update: TextUpdate) =
    result {
        let generateQuote = generateQuote getInputFile
        let! inputFile = generateQuote update.skin update.text
        let botName = context.Me.Username |> withDefault "botName"
        let caption = $"Спасибо, что пользуйетесь @{botName}"

        sendPhoto update update.chatId inputFile caption
        |> api context.Config
        |> disposeFile inputFile
        |> asyncStart

        return ()
    }


let replyToMessage context chatId replyId message =
    Funogram.Telegram.Api.sendMessageReply chatId message replyId
    |> api context.Config
    |> asyncStart

let sendMessage context chatId message =
    Funogram.Telegram.Api.sendMessage chatId message
    |> api context.Config
    |> asyncStart

let sendMessageMarkup context chatId replyId message markup =
    let req = Funogram.Telegram.Api.sendMessageMarkup chatId message markup

    { req with
        ReplyParameters = Some(ReplyParameters.Create(replyId, Int chatId)) }
    |> api context.Config
    |> asyncStart

let SET_SKIN = "setSkin"

let createChaneSkinMarkup skinsInfo =
    skinsInfo
    |> List.map (fun skinInfo ->
        [| InlineKeyboardButton.Create(to_ru skinInfo.localization, callbackData = $"{SET_SKIN},{skinInfo.name}") |])
    |> List.toArray
    |> TInlineKeyboardMarkup.Create
    |> InlineKeyboardMarkup

let sendChangeSkinMessage context chatId messageId =
    availabelSkins
    |> createChaneSkinMarkup
    |> sendMessageMarkup context chatId messageId "Доступные скины для цитат:"

let proccessCommand context (update: CommandUpdate) =
    result {
        let chatId = update.chatId
        let messageId = update.messageId
        let command = update.command
        let chatType = update.chatType
        let name = context.Me.Username |> withDefault "botName"

        match command with
        | Start ->
            startCommandsDescription name SingleChat
            |> singleStartMessage
            |> replyToMessage context chatId messageId

            return ()
        | SendChangeSkin ->
            sendChangeSkinMessage context chatId messageId
            return ()
        | Help ->
            helpCommandsDescription name chatType
            |> getHelpText
            |> replyToMessage context chatId messageId

            return ()
        | Examples ->
            do! sendExamples context chatId messageId
            return ()
    }

let resolveMessage message =
    match message with
    | Some(InaccessibleMessage _) -> sendError "Не могу получить сообщение: нет доступа"
    | Some(MaybeInaccessibleMessage.Message message) -> Ok message
    | None -> sendError "Не могу получить сообщение: нет сообщения"

let getQueryData (query: TCallbackQuery) =
    match query.Data with
    | Some data -> Ok data
    | None -> logError "Empty query"

let answerCallbackQuery id context =
    Funogram.Telegram.Api.answerCallbackQuery id "" false "" 0
    |> api context.Config
    |> asyncStart

let proccessQuery repository context (query: TCallbackQuery) =
    result {
        let! data = getQueryData query
        let! message = resolveMessage query.Message
        let chatId = message.Chat.Id
        let replyId = message.MessageId
        let parsed = split "," data
        let queryId = query.Id

        match parsed with
        | query :: skinName :: _ when query = SET_SKIN ->
            let! skinInfo = skinByName skinName |> sendErrorIfNone "Не смог сменить скин. Неизвестное имя"
            do! repository.add chatId (Some skinName)
            let newSkinName = to_ru skinInfo.localization
            replyToMessage context chatId replyId $"Смена скина произошла успешно. Новый скин для чата - {newSkinName}"
            answerCallbackQuery queryId context
            return ()
        | _ -> return! logError "Unsupported query data"
    }

type AddToChatUpdate =
    { chatId: int64
      chatName: string
      botName: string }

let proccessAddToChat (original: UpdateContext) (update: AddToChatUpdate) =
    startCommandsDescription update.botName GroupChat
    |> groupStartMessage update.botName
    |> sendMessage original update.chatId
